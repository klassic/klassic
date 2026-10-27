//! Preserve binding snapshots before static module namespacing. This pass changes
//! only names; package-wide capture transport runs after module assembly.
use klassic_span::{Diagnostic, Span};
use klassic_syntax::Expr;
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
enum Binding {
    Function(String),
    Value(String),
}
type Scope = HashMap<String, Binding>;
pub(super) struct Value {
    pub name: String,
    pub symbol: String,
}
pub(super) struct Program {
    pub expr: Expr,
    pub names: HashMap<String, String>,
    pub values: Vec<Value>,
    pub bindings: HashMap<String, String>,
}
pub(super) struct Alpha<'a> {
    text: &'a str,
    next: usize,
}
impl<'a> Alpha<'a> {
    pub fn new(text: &'a str) -> Self {
        Self { text, next: 0 }
    }
    fn fresh(&mut self, kind: &str) -> String {
        loop {
            self.next += 1;
            let name = format!("__kg_module_{kind}_{}", self.next);
            if !self.text.contains(&name) {
                return name;
            }
        }
    }
    pub fn lower(&mut self, expr: Expr) -> Result<Program, Diagnostic> {
        let span = expr.span();
        // eval_program treats a sole root block as the translation unit too.
        let items = match expr {
            Expr::Block { expressions, .. } => expressions,
            other => vec![other],
        };
        let mut program = Program {
            expr: Expr::Unit { span },
            names: HashMap::new(),
            values: Vec::new(),
            bindings: HashMap::new(),
        };
        let items = self.sequence(items, &Scope::new(), true, &mut program)?;
        let start = items.first().map_or(span.start, |item| item.span().start);
        program.expr = Expr::Block {
            expressions: items,
            span: Span::new(start, span.end.max(start)),
        };
        Ok(program)
    }
    fn sequence(
        &mut self,
        items: Vec<Expr>,
        scope: &Scope,
        top: bool,
        out: &mut Program,
    ) -> Result<Vec<Expr>, Diagnostic> {
        let mut inner = scope.clone();
        let mut own = HashSet::new();
        let mut values = HashSet::new();
        for item in &items {
            if let Expr::DefDecl { name, span, .. } = item {
                if !own.insert(name.clone()) {
                    return Err(error(
                        *span,
                        if top {
                            "a repeated top-level function definition"
                        } else {
                            "a repeated function definition"
                        },
                    ));
                }
                let target = if top {
                    name.clone()
                } else {
                    self.fresh("function")
                };
                out.names.insert(target.clone(), name.clone());
                inner.insert(name.clone(), Binding::Function(target));
            }
        }
        let mut result = Vec::with_capacity(items.len());
        for item in items {
            match item {
                Expr::DefDecl {
                    name,
                    type_params,
                    constraints,
                    params,
                    param_annotations,
                    return_annotation,
                    body,
                    span,
                } => {
                    if values.contains(&name) {
                        return Err(error(span, "a mixed function/value redefinition"));
                    }
                    let Some(Binding::Function(target)) = inner.get(&name).cloned() else {
                        return Err(error(span, "a mixed function/value redefinition"));
                    };
                    let mut body_scope = inner.clone();
                    let mut renamed = Vec::new();
                    for parameter in params {
                        let symbol = self.fresh("binding");
                        out.bindings.insert(symbol.clone(), parameter.clone());
                        body_scope.insert(parameter, Binding::Value(symbol.clone()));
                        renamed.push(symbol);
                    }
                    let body = self.expression(*body, &mut body_scope, out)?;
                    result.push(Expr::DefDecl {
                        name: target,
                        type_params,
                        constraints,
                        params: renamed,
                        param_annotations,
                        return_annotation,
                        body: Box::new(body),
                        span,
                    });
                }
                Expr::VarDecl {
                    mutable,
                    name,
                    annotation,
                    value,
                    span,
                } => {
                    if own.contains(&name) {
                        return Err(error(span, "a mixed function/value redefinition"));
                    }
                    let value = self.expression(*value, &mut inner, out)?;
                    let symbol = self.fresh("binding");
                    values.insert(name.clone());
                    out.bindings.insert(symbol.clone(), name.clone());
                    inner.insert(name.clone(), Binding::Value(symbol.clone()));
                    if top {
                        out.values.push(Value {
                            name,
                            symbol: symbol.clone(),
                        });
                    }
                    result.push(Expr::VarDecl {
                        mutable,
                        name: symbol,
                        annotation,
                        value: Box::new(value),
                        span,
                    });
                }
                Expr::ModuleHeader { .. } | Expr::Import { .. } if top => result.push(item),
                item => result.push(self.expression(item, &mut inner, out)?),
            }
        }
        Ok(result)
    }
    fn reference(name: String, scope: &Scope) -> String {
        match scope.get(&name) {
            Some(Binding::Function(target) | Binding::Value(target)) => target.clone(),
            None => name,
        }
    }
    fn expression(
        &mut self,
        expr: Expr,
        scope: &mut Scope,
        out: &mut Program,
    ) -> Result<Expr, Diagnostic> {
        Ok(match expr {
            Expr::Identifier { name, span } => Expr::Identifier {
                name: Self::reference(name, scope),
                span,
            },
            Expr::Assign { name, value, span } => Expr::Assign {
                name: Self::reference(name, scope),
                value: Box::new(self.expression(*value, scope, out)?),
                span,
            },
            Expr::Block { expressions, span } => Expr::Block {
                expressions: self.sequence(expressions, scope, false, out)?,
                span,
            },
            Expr::Unary { op, expr, span } => Expr::Unary {
                op,
                expr: Box::new(self.expression(*expr, scope, out)?),
                span,
            },
            Expr::Binary { lhs, op, rhs, span } => Expr::Binary {
                lhs: Box::new(self.expression(*lhs, scope, out)?),
                op,
                rhs: Box::new(self.expression(*rhs, scope, out)?),
                span,
            },
            Expr::Call {
                callee,
                arguments,
                span,
            } => Expr::Call {
                callee: Box::new(self.expression(*callee, scope, out)?),
                arguments: arguments
                    .into_iter()
                    .map(|arg| self.expression(arg, scope, out))
                    .collect::<Result<_, _>>()?,
                span,
            },
            Expr::RecordLiteral { fields, span } => Expr::RecordLiteral {
                fields: fields
                    .into_iter()
                    .map(|(name, value)| Ok((name, self.expression(value, scope, out)?)))
                    .collect::<Result<_, Diagnostic>>()?,
                span,
            },
            Expr::FieldAccess {
                target,
                field,
                span,
            } => Expr::FieldAccess {
                target: Box::new(self.expression(*target, scope, out)?),
                field,
                span,
            },
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                if matches!(
                    then_branch.as_ref(),
                    Expr::VarDecl { .. } | Expr::DefDecl { .. }
                ) || else_branch.as_deref().is_some_and(|branch| {
                    matches!(branch, Expr::VarDecl { .. } | Expr::DefDecl { .. })
                }) {
                    return Err(error(span, "an unbraced branch binding"));
                }
                Expr::If {
                    condition: Box::new(self.expression(*condition, scope, out)?),
                    then_branch: Box::new(self.expression(
                        *then_branch,
                        &mut scope.clone(),
                        out,
                    )?),
                    else_branch: else_branch
                        .map(|branch| {
                            self.expression(*branch, &mut scope.clone(), out)
                                .map(Box::new)
                        })
                        .transpose()?,
                    span,
                }
            }
            Expr::While {
                condition,
                body,
                span,
            } => {
                if matches!(body.as_ref(), Expr::VarDecl { .. } | Expr::DefDecl { .. }) {
                    return Err(error(span, "an unbraced loop binding"));
                }
                Expr::While {
                    condition: Box::new(self.expression(*condition, scope, out)?),
                    body: Box::new(self.expression(*body, &mut scope.clone(), out)?),
                    span,
                }
            }
            Expr::Import { span, .. } | Expr::ModuleHeader { span, .. } => {
                return Err(error(span, "a lexical import or module header"));
            }
            Expr::DefDecl { span, .. } | Expr::VarDecl { span, .. } => {
                return Err(error(span, "a declaration outside a lexical sequence"));
            }
            other => other,
        })
    }
}
fn error(span: Span, feature: &str) -> Diagnostic {
    Diagnostic::compile(
        span,
        format!("{feature} is not supported by canonical lexical module lowering yet"),
    )
}
