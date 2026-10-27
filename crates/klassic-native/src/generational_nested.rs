//! Lift lexical functions onto the canonical backend's existing direct call
//! ABI. Explicitly typed immutable captures become hidden value/root arguments;
//! mutable cells, unknown capture types and escaping function values are unsupported.
//! Declarations may follow a call-free immutable initialization prefix: all
//! evaluator placeholder cells are filled before any body can run, including
//! mutual recursion.
use std::collections::{HashMap, HashSet};

use klassic_span::{Diagnostic, SourceFile, Span};
use klassic_syntax::{Expr, TypeAnnotation};

#[derive(Clone)]
enum Binding {
    Function(String),
    Variable(String),
}
type Scope = HashMap<String, Binding>;

struct Variable {
    original: String,
    owner: usize,
    mutable: bool,
    annotation: Option<TypeAnnotation>,
}

pub(crate) struct Program {
    pub expr: Expr,
    pub function_names: HashMap<String, String>,
    pub binding_names: HashMap<String, String>,
}
struct Lowering<'a> {
    source: &'a SourceFile,
    next: usize,
    lifted: Vec<Expr>,
    names: HashMap<String, String>,
    variables: HashMap<String, Variable>,
    owners: HashMap<String, usize>,
    captures: HashMap<usize, HashSet<String>>,
}
fn unsupported(span: Span, feature: &str) -> Diagnostic {
    Diagnostic::compile(
        span,
        format!("{feature} is not supported in canonical C/LLVM mode yet"),
    )
}
impl Lowering<'_> {
    fn fresh(&mut self) -> String {
        loop {
            self.next += 1;
            let name = format!("__kg_local_{}", self.next);
            // A user binding anywhere in the original file cannot collide with
            // a generated name, even if it is declared after this lexical block.
            if !self.source.text().contains(&name) {
                return name;
            }
        }
    }
    fn variable(
        &mut self,
        name: &str,
        owner: usize,
        mutable: bool,
        annotation: Option<TypeAnnotation>,
    ) -> String {
        let symbol = self.fresh();
        self.variables.insert(
            symbol.clone(),
            Variable {
                original: name.to_owned(),
                owner,
                mutable,
                annotation,
            },
        );
        symbol
    }
    fn reference(
        &mut self,
        name: &str,
        scope: &Scope,
        owner: usize,
        span: Span,
    ) -> Result<String, Diagnostic> {
        match scope.get(name) {
            Some(Binding::Function(target)) => Ok(target.clone()),
            Some(Binding::Variable(symbol)) => {
                let variable = &self.variables[symbol];
                if variable.owner != owner {
                    if variable.mutable {
                        return Err(unsupported(
                            span,
                            &format!(
                                "a mutable nested function capture of `{}`",
                                variable.original
                            ),
                        ));
                    }
                    let Some(annotation) = &variable.annotation else {
                        return Err(unsupported(
                            span,
                            &format!(
                                "an unannotated nested function capture of `{}`",
                                variable.original
                            ),
                        ));
                    };
                    if !matches!(
                        annotation.text.trim(),
                        "Int"
                            | "Long"
                            | "Short"
                            | "Byte"
                            | "Float"
                            | "Double"
                            | "Bool"
                            | "Boolean"
                            | "String"
                    ) {
                        return Err(unsupported(
                            span,
                            &format!(
                                "a nested function capture of `{}` with type `{}`",
                                variable.original, annotation.text
                            ),
                        ));
                    }
                    self.captures
                        .entry(owner)
                        .or_default()
                        .insert(symbol.clone());
                }
                Ok(symbol.clone())
            }
            _ => Ok(name.to_owned()),
        }
    }
    fn function(&mut self, expr: Expr, scope: &Scope, target: String) -> Result<Expr, Diagnostic> {
        let Expr::DefDecl {
            name,
            type_params,
            constraints,
            params,
            param_annotations,
            return_annotation,
            body,
            span,
        } = expr
        else {
            unreachable!()
        };
        self.names.insert(target.clone(), name);
        self.next += 1;
        let owner = self.next;
        self.owners.insert(target.clone(), owner);
        let mut inner = scope.clone();
        let params = params
            .iter()
            .zip(&param_annotations)
            .map(|(parameter, annotation)| {
                let symbol = self.variable(parameter, owner, false, annotation.clone());
                inner.insert(parameter.clone(), Binding::Variable(symbol.clone()));
                symbol
            })
            .collect();
        let body = self.expression(*body, &mut inner, owner)?;
        Ok(Expr::DefDecl {
            name: target,
            type_params,
            constraints,
            params,
            param_annotations,
            return_annotation,
            body: Box::new(body),
            span,
        })
    }
    fn block(
        &mut self,
        expressions: Vec<Expr>,
        span: Span,
        scope: &Scope,
        owner: usize,
    ) -> Result<Expr, Diagnostic> {
        let mut inner = scope.clone();
        let mut seen_effect = false;
        let mut own = HashSet::new();
        for expr in &expressions {
            if let Expr::DefDecl { name, span, .. } = expr {
                if seen_effect {
                    return Err(unsupported(
                        *span,
                        "a nested function definition after an executable statement",
                    ));
                }
                if !own.insert(name.clone()) {
                    return Err(unsupported(
                        *span,
                        "a repeated function definition in one lexical scope",
                    ));
                }
                let target = self.fresh();
                inner.insert(name.clone(), Binding::Function(target));
            } else if !matches!(expr, Expr::VarDecl { mutable: false, value, .. } if pure_prefix(value))
            {
                seen_effect = true;
            }
        }
        let mut lowered = Vec::with_capacity(expressions.len());
        for expr in expressions {
            if let Expr::DefDecl { name, span, .. } = &expr {
                let Binding::Function(target) = inner.get(name).unwrap() else {
                    unreachable!()
                };
                let definition = self.function(expr.clone(), &inner, target.clone())?;
                self.lifted.push(definition);
                lowered.push(Expr::Unit { span: *span });
            } else {
                lowered.push(self.expression(expr, &mut inner, owner)?);
            }
        }
        Ok(Expr::Block {
            expressions: lowered,
            span,
        })
    }
    fn bind_captures(&mut self, expressions: &mut [Expr]) {
        let mut dependencies = HashMap::new();
        for expr in expressions.iter() {
            if let Expr::DefDecl { name, body, .. } = expr {
                let mut edges = Vec::new();
                calls(body, &mut edges);
                dependencies.insert(
                    self.owners[name],
                    edges
                        .into_iter()
                        .filter_map(|(callee, _)| self.owners.get(&callee).copied())
                        .collect::<Vec<_>>(),
                );
            }
        }
        // A sibling or descendant call may require values the caller does not
        // reference directly. Propagate those requirements to a fixed point;
        // each original immutable binding has one alpha-renamed identity.
        loop {
            let mut changed = false;
            for (caller, callees) in &dependencies {
                for callee in callees {
                    let required = self.captures.get(callee).cloned().unwrap_or_default();
                    for symbol in required {
                        if self.variables[&symbol].owner != *caller {
                            changed |= self.captures.entry(*caller).or_default().insert(symbol);
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }
        let mut signatures = HashMap::new();
        for (name, owner) in &self.owners {
            let mut captures = self
                .captures
                .get(owner)
                .into_iter()
                .flatten()
                .cloned()
                .collect::<Vec<_>>();
            captures.sort();
            signatures.insert(name.clone(), captures);
        }
        for expr in expressions.iter_mut() {
            if let Expr::DefDecl {
                name,
                params,
                param_annotations,
                ..
            } = expr
            {
                for symbol in &signatures[name] {
                    params.push(symbol.clone());
                    param_annotations.push(self.variables[symbol].annotation.clone());
                }
            }
            pass_captures(expr, &signatures);
        }
    }
    fn expression(
        &mut self,
        expr: Expr,
        scope: &mut Scope,
        owner: usize,
    ) -> Result<Expr, Diagnostic> {
        Ok(match expr {
            Expr::Identifier { name, span } => Expr::Identifier {
                name: self.reference(&name, scope, owner, span)?,
                span,
            },
            Expr::Block { expressions, span } => self.block(expressions, span, scope, owner)?,
            Expr::VarDecl {
                mutable,
                name,
                annotation,
                value,
                span,
            } => {
                let value = self.expression(*value, scope, owner)?;
                let symbol = self.variable(&name, owner, mutable, annotation.clone());
                scope.insert(name, Binding::Variable(symbol.clone()));
                Expr::VarDecl {
                    mutable,
                    name: symbol,
                    annotation,
                    value: Box::new(value),
                    span,
                }
            }
            Expr::Assign { name, value, span } => {
                let name = self.reference(&name, scope, owner, span)?;
                let value = self.expression(*value, scope, owner)?;
                Expr::Assign {
                    name,
                    value: Box::new(value),
                    span,
                }
            }
            Expr::Unary { op, expr, span } => Expr::Unary {
                op,
                expr: Box::new(self.expression(*expr, scope, owner)?),
                span,
            },
            Expr::Binary { lhs, op, rhs, span } => Expr::Binary {
                lhs: Box::new(self.expression(*lhs, scope, owner)?),
                op,
                rhs: Box::new(self.expression(*rhs, scope, owner)?),
                span,
            },
            Expr::Call {
                callee,
                arguments,
                span,
            } => {
                let callee = self.expression(*callee, scope, owner)?;
                let arguments = arguments
                    .into_iter()
                    .map(|arg| self.expression(arg, scope, owner))
                    .collect::<Result<_, _>>()?;
                Expr::Call {
                    callee: Box::new(callee),
                    arguments,
                    span,
                }
            }
            Expr::RecordLiteral { fields, span } => Expr::RecordLiteral {
                fields: fields
                    .into_iter()
                    .map(|(name, value)| Ok((name, self.expression(value, scope, owner)?)))
                    .collect::<Result<_, Diagnostic>>()?,
                span,
            },
            Expr::FieldAccess {
                target,
                field,
                span,
            } => Expr::FieldAccess {
                target: Box::new(self.expression(*target, scope, owner)?),
                field,
                span,
            },
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => Expr::If {
                condition: Box::new(self.expression(*condition, scope, owner)?),
                then_branch: Box::new(self.expression(*then_branch, &mut scope.clone(), owner)?),
                else_branch: else_branch
                    .map(|e| self.expression(*e, &mut scope.clone(), owner).map(Box::new))
                    .transpose()?,
                span,
            },
            Expr::While {
                condition,
                body,
                span,
            } => Expr::While {
                condition: Box::new(self.expression(*condition, scope, owner)?),
                body: Box::new(self.expression(*body, &mut scope.clone(), owner)?),
                span,
            },
            Expr::DefDecl { span, .. } => {
                return Err(unsupported(
                    span,
                    "a nested function definition outside a lexical block",
                ));
            }
            // Unsupported syntax is diagnosed by its existing emitter path. It
            // cannot run before a closed-function check succeeds.
            other => other,
        })
    }
}

/// Prefix values may allocate managed strings/records, but cannot invoke a
/// not-yet-initialized lexical placeholder. Reject effectful prefix initializers
/// conservatively until the general lexical initialization protocol is lowered.
fn pure_prefix(expr: &Expr) -> bool {
    match expr {
        Expr::Int { .. }
        | Expr::Double { .. }
        | Expr::Bool { .. }
        | Expr::String { .. }
        | Expr::Unit { .. }
        | Expr::Identifier { .. } => true,
        Expr::Unary { expr, .. } => pure_prefix(expr),
        Expr::Binary { lhs, rhs, .. } => pure_prefix(lhs) && pure_prefix(rhs),
        Expr::FieldAccess { target, .. } => pure_prefix(target),
        Expr::RecordLiteral { fields, .. } => fields.iter().all(|(_, value)| pure_prefix(value)),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            pure_prefix(condition)
                && pure_prefix(then_branch)
                && else_branch.as_deref().is_none_or(pure_prefix)
        }
        _ => false,
    }
}
fn pass_captures(expr: &mut Expr, signatures: &HashMap<String, Vec<String>>) {
    match expr {
        Expr::Call {
            callee,
            arguments,
            span,
        } => {
            pass_captures(callee, signatures);
            for argument in arguments.iter_mut() {
                pass_captures(argument, signatures);
            }
            if let Expr::Identifier { name, .. } = callee.as_ref()
                && let Some(required) = signatures.get(name)
            {
                arguments.extend(required.iter().map(|symbol| Expr::Identifier {
                    name: symbol.clone(),
                    span: *span,
                }));
            }
        }
        Expr::DefDecl { body, .. } => pass_captures(body, signatures),
        Expr::Block { expressions, .. } => {
            for expr in expressions {
                pass_captures(expr, signatures);
            }
        }
        Expr::VarDecl { value, .. } | Expr::Assign { value, .. } => {
            pass_captures(value, signatures)
        }
        Expr::Unary { expr, .. } => pass_captures(expr, signatures),
        Expr::Binary { lhs, rhs, .. } => {
            pass_captures(lhs, signatures);
            pass_captures(rhs, signatures);
        }
        Expr::FieldAccess { target, .. } => pass_captures(target, signatures),
        Expr::RecordLiteral { fields, .. } => {
            for (_, value) in fields {
                pass_captures(value, signatures);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            pass_captures(condition, signatures);
            pass_captures(then_branch, signatures);
            if let Some(branch) = else_branch {
                pass_captures(branch, signatures);
            }
        }
        Expr::While {
            condition, body, ..
        } => {
            pass_captures(condition, signatures);
            pass_captures(body, signatures);
        }
        _ => {}
    }
}

/// Direct call dependencies suffice for this subset: function values and
/// captured environments have no executable lowering. Inspect all branches
/// conservatively rather than executing a later definition ahead of source.
fn calls(expr: &Expr, output: &mut Vec<(String, Span)>) {
    match expr {
        Expr::Call {
            callee,
            arguments,
            span,
        } => {
            if let Expr::Identifier { name, .. } = callee.as_ref() {
                output.push((name.clone(), *span));
            }
            calls(callee, output);
            for argument in arguments {
                calls(argument, output);
            }
        }
        Expr::Block { expressions, .. } => {
            for expr in expressions {
                calls(expr, output);
            }
        }
        Expr::VarDecl { value, .. } | Expr::Assign { value, .. } => calls(value, output),
        Expr::Unary { expr, .. } => calls(expr, output),
        Expr::Binary { lhs, rhs, .. } => {
            calls(lhs, output);
            calls(rhs, output);
        }
        Expr::FieldAccess { target, .. } => calls(target, output),
        Expr::RecordLiteral { fields, .. } => {
            for (_, value) in fields {
                calls(value, output);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            calls(condition, output);
            calls(then_branch, output);
            if let Some(branch) = else_branch {
                calls(branch, output);
            }
        }
        Expr::While {
            condition, body, ..
        } => {
            calls(condition, output);
            calls(body, output);
        }
        // A definition does not execute its body. All lifted declarations are
        // separately indexed below, including bodies that call sibling defs.
        _ => {}
    }
}
fn validate_initialization(
    expressions: &[Expr],
    top_level: &HashSet<String>,
) -> Result<(), Diagnostic> {
    let mut dependencies = HashMap::new();
    for expr in expressions {
        if let Expr::DefDecl { name, body, .. } = expr {
            let mut edges = Vec::new();
            calls(body, &mut edges);
            dependencies.insert(name, edges);
        }
    }
    let mut initialized = HashSet::new();
    for expr in expressions {
        if let Expr::DefDecl { name, .. } = expr {
            initialized.insert(name);
            continue;
        }
        let mut entries = Vec::new();
        calls(expr, &mut entries);
        for (entry, span) in entries {
            let mut pending = vec![entry];
            let mut visited = HashSet::new();
            while let Some(name) = pending.pop() {
                if !visited.insert(name.clone()) {
                    continue;
                }
                if top_level.contains(&name) && !initialized.contains(&name) {
                    return Err(unsupported(
                        span,
                        &format!("a call before a referenced function definition `{name}`"),
                    ));
                }
                if let Some(edges) = dependencies.get(&name) {
                    pending.extend(edges.iter().map(|(callee, _)| callee.clone()));
                }
            }
        }
    }
    Ok(())
}

pub(crate) fn lower(expr: Expr, source: &SourceFile) -> Result<Program, Diagnostic> {
    let span = expr.span();
    let expressions = match expr {
        // parse_source returns its sole expression directly. A braced block's
        // span includes `{`, unlike a synthetic program sequence whose span
        // starts at the first expression. Keep that lexical scope intact.
        Expr::Block { expressions, span }
            if expressions
                .first()
                .is_none_or(|first| first.span().start > span.start) =>
        {
            vec![Expr::Block { expressions, span }]
        }
        Expr::Block { expressions, .. } => expressions,
        other => vec![other],
    };
    let mut scope = Scope::new();
    for expr in &expressions {
        if let Expr::DefDecl { name, span, .. } = expr
            && scope
                .insert(name.clone(), Binding::Function(name.clone()))
                .is_some()
        {
            return Err(unsupported(
                *span,
                "a repeated top-level function definition",
            ));
        }
    }
    let top_level = scope.keys().cloned().collect::<HashSet<_>>();
    let mut lowering = Lowering {
        source,
        next: 0,
        lifted: Vec::new(),
        names: HashMap::new(),
        variables: HashMap::new(),
        owners: HashMap::new(),
        captures: HashMap::new(),
    };
    let mut output = Vec::with_capacity(expressions.len());
    for expr in expressions {
        if let Expr::DefDecl { name, .. } = &expr {
            output.push(lowering.function(expr.clone(), &scope, name.clone())?);
        } else {
            output.push(lowering.expression(expr, &mut scope, 0)?);
        }
    }
    output.append(&mut lowering.lifted);
    lowering.bind_captures(&mut output);
    validate_initialization(&output, &top_level)?;
    Ok(Program {
        expr: Expr::Block {
            expressions: output,
            span,
        },
        function_names: lowering.names,
        binding_names: lowering
            .variables
            .into_iter()
            .map(|(symbol, variable)| (symbol, variable.original))
            .collect(),
    })
}
