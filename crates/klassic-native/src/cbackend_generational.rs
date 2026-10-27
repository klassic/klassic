//! Structured C lowering for the canonical GC ABI. Every managed operand is a
//! RootCell pointer; integer and floating fields live in separately described
//! scalar leaves so reference descriptors never inspect scalar bit patterns.
use super::{CType, ctype_from_annotation, unsupported};
use klassic_span::{Diagnostic, Span};
use klassic_syntax::{BinaryOp, Expr, UnaryOp};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Ty {
    Int,
    Double,
    Float,
    Bool,
    String,
    Record(usize),
    Unit,
}
impl Ty {
    fn c(self) -> &'static str {
        match self {
            Self::Int | Self::Bool => "int64_t",
            Self::Double => "double",
            Self::Float => "float",
            Self::String | Self::Record(_) => "RootCell *",
            Self::Unit => "void",
        }
    }
    fn managed(self) -> bool {
        matches!(self, Self::String | Self::Record(_))
    }
}
#[derive(Clone)]
struct Value {
    code: String,
    ty: Ty,
}
#[derive(Default)]
struct Scope {
    locals: Vec<(String, Value)>,
    roots: Vec<String>,
}
#[derive(Clone)]
struct Function {
    name: String,
    params: Vec<(String, Ty)>,
    ret: Ty,
    body: Expr,
}
#[derive(Default)]
struct Emitter {
    functions: Vec<Function>,
    shapes: Vec<Vec<(String, Ty)>>,
    scopes: Vec<Scope>,
    roots: Vec<String>,
    code: String,
    indent: usize,
    next: u64,
    source: Option<crate::generational_modules::Sources>,
    current_function: Option<String>,
}
impl Emitter {
    fn fresh(&mut self) -> String {
        let n = self.next;
        self.next += 1;
        format!("kg_v_{n}")
    }
    fn line(&mut self, text: &str) {
        self.code.push_str(&"    ".repeat(self.indent));
        self.code.push_str(text);
        self.code.push('\n');
    }
    fn root(&mut self) -> Value {
        let code = self.fresh();
        self.roots.push(code.clone());
        self.scopes.last_mut().unwrap().roots.push(code.clone());
        Value { code, ty: Ty::Unit }
    }
    fn open(&mut self) {
        self.scopes.push(Scope::default());
    }
    fn close(&mut self) {
        for r in self.scopes.pop().unwrap().roots.into_iter().rev() {
            self.line(&format!("kg_root_clear({r});"));
        }
    }
    fn lookup(&self, name: &str, span: Span) -> Result<Value, Diagnostic> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| {
                s.locals
                    .iter()
                    .rev()
                    .find(|(n, _)| n == name)
                    .map(|(_, v)| v.clone())
            })
            .ok_or_else(|| unsupported(span, &format!("identifier `{name}`")))
    }
    fn temp(&mut self, ty: Ty, expression: &str) -> Value {
        let code = self.fresh();
        self.line(&format!("{} {code} = {expression};", ty.c()));
        Value { code, ty }
    }
    fn copy(&mut self, out: &Value, value: &Value) {
        if out.ty.managed() {
            self.line(&format!("kg_root_copy({}, {});", out.code, value.code));
        } else {
            self.line(&format!("{} = {};", out.code, value.code));
        }
    }
    fn result(&mut self, ty: Ty) -> Value {
        if ty.managed() {
            let mut v = self.root();
            v.ty = ty;
            v
        } else {
            let code = self.fresh();
            self.line(&format!("{} {code};", ty.c()));
            Value { code, ty }
        }
    }
    fn error_prefix(&self, span: Span) -> (String, usize) {
        let prefix = self
            .source
            .as_ref()
            .expect("canonical source context")
            .prefix(span, self.current_function.clone());
        let bytes = prefix
            .as_bytes()
            .iter()
            .map(|b| format!("\\x{b:02x}"))
            .collect::<String>();
        (format!("\"{bytes}\""), prefix.len())
    }
    fn expression(&mut self, e: &Expr) -> Result<Value, Diagnostic> {
        match e {
            Expr::Int { value, .. } => Ok(Value {
                code: if *value == i64::MIN {
                    "(-INT64_C(9223372036854775807) - INT64_C(1))".into()
                } else {
                    format!("INT64_C({value})")
                },
                ty: Ty::Int,
            }),
            Expr::Double {
                value,
                kind: klassic_syntax::FloatLiteralKind::Float,
                ..
            } => Ok(Value {
                code: format!("kg_bits_f32(UINT32_C({}))", (*value as f32).to_bits()),
                ty: Ty::Float,
            }),
            Expr::Double { value, .. } => Ok(Value {
                code: if value.is_finite() {
                    format!("{value:?}")
                } else {
                    format!("kg_bits_f64(UINT64_C({}))", value.to_bits())
                },
                ty: Ty::Double,
            }),
            Expr::Unit { .. } => Ok(Value {
                code: String::new(),
                ty: Ty::Unit,
            }),
            Expr::Bool { value, .. } => Ok(Value {
                code: if *value { "1" } else { "0" }.into(),
                ty: Ty::Bool,
            }),
            Expr::String { value, .. } => {
                let out = self.result(Ty::String);
                let bytes = value
                    .as_bytes()
                    .iter()
                    .map(|b| format!("\\x{b:02x}"))
                    .collect::<String>();
                self.line(&format!(
                    "kg_string_from_utf8((const unsigned char *)\"{bytes}\", UINT64_C({}), {});",
                    value.len(),
                    out.code
                ));
                Ok(out)
            }
            Expr::Identifier { name, span } => self.lookup(name, *span),
            Expr::Unary { op, expr, span } => {
                let value = self.expression(expr)?;
                let code = match (op, value.ty) {
                    (UnaryOp::Plus, Ty::Int | Ty::Double | Ty::Float) => return Ok(value),
                    (UnaryOp::Minus, Ty::Int) => {
                        let (prefix, length) = self.error_prefix(*span);
                        format!(
                            "kg_i64_neg_at({}, {prefix}, UINT64_C({length}))",
                            value.code
                        )
                    }
                    (UnaryOp::Minus, Ty::Double | Ty::Float) => format!("-({})", value.code),
                    (UnaryOp::Not, Ty::Bool) => format!("!({})", value.code),
                    _ => return Err(unsupported(*span, "a unary operand of this type")),
                };
                Ok(self.temp(value.ty, &code))
            }
            Expr::Binary { lhs, op, rhs, span } => {
                let left = self.expression(lhs)?;
                // Capture the left operand before evaluating a potentially
                // allocating or mutating right operand. Scalar values are native.
                let left = if left.ty.managed() {
                    let out = self.result(left.ty);
                    self.copy(&out, &left);
                    out
                } else {
                    self.temp(left.ty, &left.code)
                };
                if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
                    let out = self.result(Ty::Bool);
                    let test = if *op == BinaryOp::LogicalAnd {
                        left.code.clone()
                    } else {
                        format!("!({})", left.code)
                    };
                    self.line(&format!("if ({test}) {{"));
                    self.indent += 1;
                    self.open();
                    let right = self.expression(rhs)?;
                    self.copy(&out, &right);
                    self.close();
                    self.indent -= 1;
                    self.line("} else {");
                    self.indent += 1;
                    self.copy(&out, &left);
                    self.indent -= 1;
                    self.line("}");
                    return Ok(out);
                }
                let right = self.expression(rhs)?;
                let (left, right) = if left.ty != right.ty
                    && matches!(left.ty, Ty::Int | Ty::Float | Ty::Double)
                    && matches!(right.ty, Ty::Int | Ty::Float | Ty::Double)
                {
                    let promoted = if left.ty == Ty::Double || right.ty == Ty::Double {
                        Ty::Double
                    } else {
                        Ty::Float
                    };
                    let left = if left.ty != promoted {
                        self.temp(promoted, &format!("({})({})", promoted.c(), left.code))
                    } else {
                        left
                    };
                    let right = if right.ty != promoted {
                        self.temp(promoted, &format!("({})({})", promoted.c(), right.code))
                    } else {
                        right
                    };
                    (left, right)
                } else {
                    (left, right)
                };
                if left.ty != right.ty {
                    return Err(unsupported(*span, "mixed binary operand types"));
                }
                if left.ty == Ty::String {
                    if *op == BinaryOp::Add {
                        let out = self.result(Ty::String);
                        self.line(&format!(
                            "kg_string_concat({}, {}, {});",
                            left.code, right.code, out.code
                        ));
                        return Ok(out);
                    }
                    let compare = self.temp(
                        Ty::Int,
                        &format!("kg_string_compare({}, {})", left.code, right.code),
                    );
                    let operator = match op {
                        BinaryOp::Equal => "==",
                        BinaryOp::NotEqual => "!=",
                        BinaryOp::Less => "<",
                        BinaryOp::LessEqual => "<=",
                        BinaryOp::Greater => ">",
                        BinaryOp::GreaterEqual => ">=",
                        _ => return Err(unsupported(*span, "this string operator")),
                    };
                    return Ok(self.temp(Ty::Bool, &format!("({} {operator} 0)", compare.code)));
                }
                if left.ty.managed() {
                    return Err(unsupported(*span, "this managed binary operand"));
                }
                if left.ty == Ty::Int {
                    let helper = match op {
                        BinaryOp::Add => Some("add"),
                        BinaryOp::Subtract => Some("sub"),
                        BinaryOp::Multiply => Some("mul"),
                        BinaryOp::Divide => Some("div"),
                        _ => None,
                    };
                    if let Some(helper) = helper {
                        let (prefix, length) = self.error_prefix(*span);
                        return Ok(self.temp(
                            Ty::Int,
                            &format!(
                                "kg_i64_{helper}_at({}, {}, {prefix}, UINT64_C({length}))",
                                left.code, right.code
                            ),
                        ));
                    }
                }
                let (operator, ty) = match op {
                    BinaryOp::Add => ("+", left.ty),
                    BinaryOp::Subtract => ("-", left.ty),
                    BinaryOp::Multiply => ("*", left.ty),
                    BinaryOp::Divide => ("/", left.ty),
                    BinaryOp::Less => ("<", Ty::Bool),
                    BinaryOp::LessEqual => ("<=", Ty::Bool),
                    BinaryOp::Greater => (">", Ty::Bool),
                    BinaryOp::GreaterEqual => (">=", Ty::Bool),
                    BinaryOp::Equal => ("==", Ty::Bool),
                    BinaryOp::NotEqual => ("!=", Ty::Bool),
                    BinaryOp::LogicalAnd => ("&&", Ty::Bool),
                    BinaryOp::LogicalOr => ("||", Ty::Bool),
                    _ => return Err(unsupported(*span, "this binary operator")),
                };
                Ok(self.temp(ty, &format!("({} {operator} {})", left.code, right.code)))
            }
            Expr::RecordLiteral { fields, span } => {
                if fields.is_empty() {
                    return Err(unsupported(*span, "an empty record"));
                }
                let mut record = self.root();
                let mut leaves = Vec::new();
                let mut shape = Vec::new();
                self.line(&format!("kg_root_clear({});", record.code));
                for (name, expr) in fields {
                    let value = self.expression(expr)?;
                    if value.ty == Ty::Unit {
                        return Err(unsupported(expr.span(), "a unit record field"));
                    }
                    let leaf = if value.ty.managed() {
                        let out = self.result(value.ty);
                        self.copy(&out, &value);
                        out
                    } else {
                        let bits = if value.ty == Ty::Float {
                            format!("kg_f32_bits({})", value.code)
                        } else if value.ty == Ty::Double {
                            format!("kg_f64_bits({})", value.code)
                        } else {
                            format!("(uint64_t)({})", value.code)
                        };
                        let leaf = self.root();
                        self.line(&format!(
                            "kg_alloc_into(UINT64_C(8), UINT64_C(1), {});",
                            leaf.code
                        ));
                        self.line(&format!(
                            "kg_store_i64({}, UINT64_C(0), {bits});",
                            leaf.code
                        ));
                        leaf
                    };
                    leaves.push(leaf);
                    shape.push((name.clone(), value.ty));
                }
                self.line(&format!(
                    "kg_alloc_into(UINT64_C({}), UINT64_C(2), {});",
                    fields.len() * 8,
                    record.code
                ));
                for (i, leaf) in leaves.into_iter().enumerate() {
                    self.line(&format!(
                        "kg_store_ref({}, UINT64_C({}), {});",
                        record.code,
                        i * 8,
                        leaf.code
                    ));
                    self.line(&format!("kg_root_clear({});", leaf.code));
                }
                let id = self
                    .shapes
                    .iter()
                    .position(|s| s == &shape)
                    .unwrap_or_else(|| {
                        self.shapes.push(shape);
                        self.shapes.len() - 1
                    });
                record.ty = Ty::Record(id);
                Ok(record)
            }
            Expr::FieldAccess {
                target,
                field,
                span,
            } => {
                let base = self.expression(target)?;
                let Ty::Record(shape) = base.ty else {
                    return Err(unsupported(*span, "a field on a non-record"));
                };
                let Some((index, (_, ty))) = self.shapes[shape]
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == field)
                else {
                    return Err(unsupported(*span, "an unknown record field"));
                };
                let (index, ty) = (index, *ty);
                let leaf = self.root();
                self.line(&format!(
                    "kg_load_ref_into({}, UINT64_C({}), {});",
                    base.code,
                    index * 8,
                    leaf.code
                ));
                if ty.managed() {
                    return Ok(Value {
                        code: leaf.code,
                        ty,
                    });
                }
                let bits = self.fresh();
                self.line(&format!(
                    "uint64_t {bits} = kg_load_i64({}, UINT64_C(0));",
                    leaf.code
                ));
                self.line(&format!("kg_root_clear({});", leaf.code));
                let code = if ty == Ty::Float {
                    format!("kg_bits_f32((uint32_t){bits})")
                } else if ty == Ty::Double {
                    format!("kg_bits_f64({bits})")
                } else {
                    format!("(int64_t){bits}")
                };
                Ok(self.temp(ty, &code))
            }
            Expr::Block { expressions, .. } => {
                self.open();
                let mut value = Value {
                    code: String::new(),
                    ty: Ty::Unit,
                };
                for (i, sub) in expressions.iter().enumerate() {
                    if i + 1 == expressions.len() {
                        value = self.expression_or_statement(sub)?;
                    } else {
                        self.statement(sub)?;
                    }
                }
                if value.ty.managed() {
                    let mut out = self.root();
                    out.ty = value.ty;
                    self.scopes.last_mut().unwrap().roots.pop();
                    let parent = self.scopes.len() - 2;
                    self.scopes[parent].roots.push(out.code.clone());
                    self.copy(&out, &value);
                    value = out;
                }
                self.close();
                Ok(value)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let Some(other) = else_branch else {
                    return Err(unsupported(*span, "if without else as a value"));
                };
                let cond = self.expression(condition)?;
                // Lower branches into separate buffers so their result type can
                // determine a destination declared before the runtime branch.
                let before = std::mem::take(&mut self.code);
                self.open();
                let yes = self.expression(then_branch)?;
                let yes_code = std::mem::take(&mut self.code);
                // Keep branch roots live until copying its result below.
                let yes_scope = self.scopes.pop().unwrap();
                self.open();
                let no = self.expression(other)?;
                let no_code = std::mem::take(&mut self.code);
                let no_scope = self.scopes.pop().unwrap();
                self.code = before;
                if yes.ty != no.ty || yes.ty == Ty::Unit {
                    return Err(unsupported(*span, "incompatible if value branches"));
                }
                let out = self.result(yes.ty);
                self.line(&format!("if ({}) {{", cond.code));
                self.indent += 1;
                self.code.push_str(&yes_code);
                self.copy(&out, &yes);
                for r in yes_scope.roots.into_iter().rev() {
                    self.line(&format!("kg_root_clear({r});"));
                }
                self.indent -= 1;
                self.line("} else {");
                self.indent += 1;
                self.code.push_str(&no_code);
                self.copy(&out, &no);
                for r in no_scope.roots.into_iter().rev() {
                    self.line(&format!("kg_root_clear({r});"));
                }
                self.indent -= 1;
                self.line("}");
                Ok(out)
            }
            Expr::Call {
                callee,
                arguments,
                span,
            } => {
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return Err(unsupported(*span, "a non-identifier call"));
                };
                if name == "length"
                    && arguments.len() == 1
                    && !self.functions.iter().any(|f| f.name == *name)
                {
                    let value = self.expression(&arguments[0])?;
                    if value.ty != Ty::String {
                        return Err(unsupported(*span, "length of this type"));
                    }
                    return Ok(self.temp(
                        Ty::Int,
                        &format!("(int64_t)kg_string_length({})", value.code),
                    ));
                }
                let Some(function) = self.functions.iter().find(|f| &f.name == name).cloned()
                else {
                    return Err(unsupported(*span, &format!("function `{name}`")));
                };
                if arguments.len() != function.params.len() {
                    return Err(unsupported(*span, "a function arity mismatch"));
                }
                let mut values = Vec::new();
                for (arg, (_, expected)) in arguments.iter().zip(&function.params) {
                    let v = self.expression(arg)?;
                    if v.ty != *expected {
                        return Err(unsupported(arg.span(), "an argument type"));
                    }
                    let v = if v.ty.managed() {
                        let out = self.result(v.ty);
                        self.copy(&out, &v);
                        out
                    } else {
                        self.temp(v.ty, &v.code)
                    };
                    values.push(v.code);
                }
                if function.ret.managed() {
                    let out = self.result(function.ret);
                    values.push(out.code.clone());
                    self.line(&format!("kl_{}({});", name, values.join(", ")));
                    return Ok(out);
                }
                let call = format!("kl_{}({})", name, values.join(", "));
                if function.ret == Ty::Unit {
                    self.line(&format!("{call};"));
                    Ok(Value {
                        code: String::new(),
                        ty: Ty::Unit,
                    })
                } else {
                    Ok(self.temp(function.ret, &call))
                }
            }
            _ => Err(unsupported(
                e.span(),
                "this canonical generational expression",
            )),
        }
    }
    fn expression_or_statement(&mut self, e: &Expr) -> Result<Value, Diagnostic> {
        if matches!(
            e,
            Expr::VarDecl { .. }
                | Expr::Assign { .. }
                | Expr::While { .. }
                | Expr::DefDecl { .. }
                | Expr::ModuleHeader { .. }
                | Expr::Import { .. }
        ) || (matches!(e, Expr::Call{callee,..} if matches!(callee.as_ref(),Expr::Identifier{name,..} if name=="println"))
            && !self.functions.iter().any(|f| f.name == "println"))
        {
            self.statement(e)?;
            Ok(Value {
                code: String::new(),
                ty: Ty::Unit,
            })
        } else {
            self.expression(e)
        }
    }
    fn statement(&mut self, e: &Expr) -> Result<(), Diagnostic> {
        match e {
            Expr::VarDecl {
                name, value, span, ..
            } => {
                let value = self.expression(value)?;
                if value.ty == Ty::Unit {
                    return Err(unsupported(*span, "a Unit local binding"));
                }
                let out = if value.ty.managed() {
                    let mut v = self.root();
                    v.ty = value.ty;
                    self.copy(&v, &value);
                    v
                } else {
                    self.temp(value.ty, &value.code)
                };
                self.scopes
                    .last_mut()
                    .unwrap()
                    .locals
                    .push((name.clone(), out));
            }
            Expr::Assign { name, value, span } => {
                let out = self.lookup(name, *span)?;
                let value = self.expression(value)?;
                if out.ty != value.ty {
                    return Err(unsupported(*span, "an assignment type"));
                }
                self.copy(&out, &value);
            }
            Expr::Block { expressions, .. } => {
                self.open();
                for sub in expressions {
                    self.statement(sub)?;
                }
                self.close();
            }
            Expr::While {
                condition, body, ..
            } => {
                self.line("for (;;) {");
                self.indent += 1;
                self.line("kg_poll();");
                let cond = self.expression(condition)?;
                self.line(&format!("if (!({})) break;", cond.code));
                self.open();
                self.statement(body)?;
                self.close();
                self.indent -= 1;
                self.line("}");
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond = self.expression(condition)?;
                self.line(&format!("if ({}) {{", cond.code));
                self.indent += 1;
                self.open();
                self.statement(then_branch)?;
                self.close();
                self.indent -= 1;
                if let Some(other) = else_branch {
                    self.line("} else {");
                    self.indent += 1;
                    self.open();
                    self.statement(other)?;
                    self.close();
                    self.indent -= 1;
                }
                self.line("}");
            }
            Expr::Call {
                callee,
                arguments,
                span,
            } if matches!(callee.as_ref(),Expr::Identifier{name,..} if name=="println")
                && !self.functions.iter().any(|f| f.name == "println") =>
            {
                if arguments.len() != 1 {
                    return Err(unsupported(*span, "println arity"));
                }
                let value = self.expression(&arguments[0])?;
                if value.ty == Ty::String {
                    self.line(&format!("kg_string_println({});", value.code));
                    return Ok(());
                }
                let printer = match value.ty {
                    Ty::Int => "i64",
                    Ty::Double => "f64",
                    Ty::Float => "f32",
                    Ty::Bool => "bool",
                    _ => return Err(unsupported(*span, "printing this heap value")),
                };
                let value = self.temp(value.ty, &value.code);
                self.line("kg_native_enter();");
                self.line(&format!("klassic_rt_println_{printer}({});", value.code));
                self.line("kg_native_exit();");
            }
            Expr::DefDecl { span, .. } => {
                return Err(unsupported(
                    *span,
                    "a nested function definition in canonical mode",
                ));
            }
            Expr::Import { span, .. } | Expr::ModuleHeader { span, .. } => {
                return Err(unsupported(*span, "modules and imports in canonical mode"));
            }
            _ => {
                let value = self.expression(e)?;
                if value.ty != Ty::Unit {
                    self.line(&format!("(void)({});", value.code));
                }
            }
        }
        Ok(())
    }
    fn frame(&mut self) -> String {
        let mut out = String::new();
        for r in &self.roots {
            out.push_str(&format!("    RootCell *{r} = kg_root_new();\n"));
        }
        out.push_str(&self.code);
        out
    }
    fn finish(&mut self) {
        for r in self.roots.clone().into_iter().rev() {
            self.line(&format!("kg_root_release({r});"));
        }
    }
    fn reset(&mut self) {
        self.current_function = None;
        self.scopes.clear();
        self.roots.clear();
        self.code.clear();
        self.indent = 1;
        self.open();
    }
}
fn ty(text: &str, span: Span) -> Result<Ty, Diagnostic> {
    if text.trim() == "Float" {
        return Ok(Ty::Float);
    }
    match ctype_from_annotation(text, span)? {
        CType::Int => Ok(Ty::Int),
        CType::Double => Ok(Ty::Double),
        CType::Bool => Ok(Ty::Bool),
        CType::Unit => Ok(Ty::Unit),
        CType::Str => Ok(Ty::String),
    }
}
pub(super) fn emit(
    expr: &Expr,
    c: crate::GenerationalBackendConfig,
    source: &crate::generational_modules::Sources,
    function_names: &std::collections::HashMap<String, String>,
) -> Result<String, Diagnostic> {
    let mut e = Emitter {
        source: Some(source.clone()),
        ..Default::default()
    };
    let statements: Vec<&Expr> = if let Expr::Block { expressions, .. } = expr {
        expressions.iter().collect()
    } else {
        vec![expr]
    };
    for statement in &statements {
        if let Expr::DefDecl {
            name,
            params,
            param_annotations,
            return_annotation,
            body,
            span,
            ..
        } = statement
        {
            let mut ps = Vec::new();
            for (name, annotation) in params.iter().zip(param_annotations) {
                let Some(a) = annotation else {
                    return Err(unsupported(*span, "an unannotated function parameter"));
                };
                let parameter = ty(&a.text, *span)?;
                if parameter == Ty::Unit {
                    return Err(unsupported(
                        *span,
                        "a Unit function parameter in canonical mode",
                    ));
                }
                ps.push((name.clone(), parameter));
            }
            let Some(a) = return_annotation else {
                return Err(unsupported(*span, "an unannotated function return"));
            };
            e.functions.push(Function {
                name: name.clone(),
                params: ps,
                ret: ty(&a.text, *span)?,
                body: body.as_ref().clone(),
            });
        }
    }
    let mut out = String::from(
        "/* generated canonical RootCell C; link the shared generational runtime */\n#include <stdint.h>\n#include <string.h>\n#include \"klassic_gen_runtime.h\"\n#include \"klassic_gen_string.h\"\n#include \"klassic_gen_scalar.h\"\nextern void klassic_rt_println_i64(int64_t);\nextern void klassic_rt_println_f64(double);\nextern void klassic_rt_println_f32(float);\nextern void klassic_rt_println_bool(int64_t);\n",
    );
    let header_end = out.len();
    let signature = |f: &Function| {
        let mut params = f
            .params
            .iter()
            .map(|(n, t)| format!("{} kl_{n}", t.c()))
            .collect::<Vec<_>>();
        if f.ret.managed() {
            params.push("RootCell *kg_return_out".into());
        }
        format!(
            "{} kl_{}({})",
            if f.ret.managed() { "void" } else { f.ret.c() },
            f.name,
            if params.is_empty() {
                "void".into()
            } else {
                params.join(", ")
            }
        )
    };
    for f in &e.functions {
        out.push_str(&format!("{};\n", signature(f)));
    }
    for f in e.functions.clone() {
        e.reset();
        e.current_function = Some(function_names.get(&f.name).unwrap_or(&f.name).clone());
        for (n, t) in &f.params {
            let incoming = Value {
                code: format!("kl_{n}"),
                ty: *t,
            };
            let value = if t.managed() {
                let out = e.result(*t);
                e.copy(&out, &incoming);
                out
            } else {
                incoming
            };
            e.scopes.last_mut().unwrap().locals.push((n.clone(), value));
        }
        e.line("kg_poll();");
        let value = e.expression_or_statement(&f.body)?;
        if value.ty != f.ret {
            return Err(unsupported(f.body.span(), "a function result type"));
        }
        let value = if f.ret.managed() {
            e.line(&format!("kg_root_copy(kg_return_out, {});", value.code));
            None
        } else if value.ty != Ty::Unit {
            Some(e.temp(value.ty, &value.code))
        } else {
            None
        };
        e.close();
        e.finish();
        if let Some(value) = value {
            e.line(&format!("return {};", value.code));
        }
        out.push_str(&format!("{} {{\n{}}}\n", signature(&f), e.frame()));
    }
    e.reset();
    e.line("kg_poll();");
    for statement in statements {
        if !matches!(statement, Expr::DefDecl { .. }) {
            e.statement(statement)?;
        }
    }
    e.close();
    e.finish();
    e.line("kg_shutdown();");
    e.line("return 0;");
    let delay = c
        .uncommit_delay_ns
        .map(|ns| format!("    kg_configure_uncommit_delay(UINT64_C({ns}));\n"))
        .unwrap_or_default();
    out.push_str(&format!("int main(void) {{\n    kg_configure(UINT64_C({}), UINT64_C({}), UINT64_C({}), UINT64_C({}));\n{delay}    kg_init();\n{}}}\n",c.hard_max,c.min_heap,c.soft_max,c.flags,e.frame()));
    if out.contains("kg_f32_bits(") {
        out.insert_str(header_end,"static inline uint32_t kg_f32_bits(float v) { uint32_t b; memcpy(&b, &v, sizeof b); return b; }\n");
    }
    if out.contains("kg_bits_f32(") {
        out.insert_str(header_end,"static inline float kg_bits_f32(uint32_t b) { float v; memcpy(&v, &b, sizeof v); return v; }\n");
    }
    if out.contains("kg_f64_bits(") {
        out.insert_str(header_end,"static inline uint64_t kg_f64_bits(double v) { uint64_t b; memcpy(&b, &v, sizeof b); return b; }\n");
    }
    if out.contains("kg_bits_f64(") {
        out.insert_str(header_end,"static inline double kg_bits_f64(uint64_t b) { double v; memcpy(&v, &b, sizeof v); return v; }\n");
    }
    Ok(out)
}
