//! Portable C backend (roadmap PR 9 / PR 10). Emits a single C
//! translation unit for a growing Klassic subset: `Int` / `Bool` /
//! `String` values, arithmetic / comparison / logical operators,
//! string concatenation / equality / `length` / `substring` / `at` /
//! `toString`, `val` / `mutable` / assignment, `if` / `while`,
//! `println`, and annotated top-level `def`s (including recursion).
//! Strings are `KStr` values served by the `klassic_rt_*` shims in
//! `klassic-runtime` (built as `libklassic_runtime.a`), which share
//! their semantics with the evaluator by construction. Anything
//! outside the subset fails with a source-located diagnostic — never
//! wrong code. The direct ELF path stays untouched.

use klassic_span::{Diagnostic, Span};
use klassic_syntax::{BinaryOp, Expr};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CType {
    Int,
    Double,
    Bool,
    Str,
    Unit,
}

impl CType {
    fn c_name(self) -> &'static str {
        match self {
            CType::Int => "int64_t",
            CType::Double => "double",
            CType::Bool => "int64_t",
            CType::Str => "KStr",
            CType::Unit => "void",
        }
    }
}

fn unsupported(span: Span, feature: &str) -> Diagnostic {
    Diagnostic::compile(
        span,
        format!("{feature} is not supported by the portable C backend yet"),
    )
}

fn ctype_from_annotation(text: &str, span: Span) -> Result<CType, Diagnostic> {
    match text.trim() {
        "Int" | "Long" | "Short" | "Byte" => Ok(CType::Int),
        "Double" | "Float" => Ok(CType::Double),
        "Bool" | "Boolean" => Ok(CType::Bool),
        "String" => Ok(CType::Str),
        "Unit" => Ok(CType::Unit),
        other => Err(unsupported(span, &format!("type annotation `{other}`"))),
    }
}

fn escape_c_string(text: &str) -> String {
    let mut out = String::with_capacity(text.len() + 2);
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            other => out.push(other),
        }
    }
    out
}

/// Klassic identifiers are C-compatible except for the risk of
/// colliding with C keywords / runtime names; prefix everything.
fn c_ident(name: &str) -> String {
    format!("kl_{name}")
}

struct CFunction {
    name: String,
    params: Vec<(String, CType)>,
    ret: CType,
    body: Expr,
}

#[derive(Default)]
struct CEmitter {
    functions: Vec<CFunction>,
    locals: Vec<Vec<(String, CType)>>,
    body: String,
    indent: usize,
}

impl CEmitter {
    fn lookup(&self, name: &str) -> Option<CType> {
        for scope in self.locals.iter().rev() {
            if let Some((_, ty)) = scope.iter().rev().find(|(n, _)| n == name) {
                return Some(*ty);
            }
        }
        None
    }

    fn function(&self, name: &str) -> Option<&CFunction> {
        self.functions.iter().find(|f| f.name == name)
    }

    fn line(&mut self, text: &str) {
        for _ in 0..self.indent {
            self.body.push_str("    ");
        }
        self.body.push_str(text);
        self.body.push('\n');
    }

    /// Emit `expr` as a C expression string, returning its type.
    fn expr(&mut self, expr: &Expr) -> Result<(String, CType), Diagnostic> {
        match expr {
            Expr::Int { value, .. } => Ok((format!("INT64_C({value})"), CType::Int)),
            Expr::Double { value, .. } => {
                // {value:?} keeps a trailing .0 so C parses a double.
                Ok((format!("{value:?}"), CType::Double))
            }
            Expr::Bool { value, .. } => Ok((if *value { "1" } else { "0" }.into(), CType::Bool)),
            Expr::String { value, span } => {
                if value.contains("#{") {
                    return Err(unsupported(*span, "string interpolation"));
                }
                Ok((
                    format!("KL_LIT(\"{}\", {})", escape_c_string(value), value.len()),
                    CType::Str,
                ))
            }
            Expr::Identifier { name, span } => self
                .lookup(name)
                .map(|ty| (c_ident(name), ty))
                .ok_or_else(|| unsupported(*span, &format!("identifier `{name}`"))),
            Expr::Unary { .. } => Err(unsupported(expr.span(), "unary operator")),
            Expr::Binary { lhs, op, rhs, span } => {
                let (left, left_ty) = self.expr(lhs)?;
                let (right, right_ty) = self.expr(rhs)?;
                let c_op = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Subtract => "-",
                    BinaryOp::Multiply => "*",
                    BinaryOp::Divide => "/",
                    BinaryOp::Less => "<",
                    BinaryOp::LessEqual => "<=",
                    BinaryOp::Greater => ">",
                    BinaryOp::GreaterEqual => ">=",
                    BinaryOp::Equal => "==",
                    BinaryOp::NotEqual => "!=",
                    BinaryOp::LogicalAnd => "&&",
                    BinaryOp::LogicalOr => "||",
                    _ => return Err(unsupported(*span, "this binary operator")),
                };
                if left_ty == CType::Str || right_ty == CType::Str {
                    if left_ty != right_ty {
                        return Err(unsupported(*span, "mixed string/non-string operands"));
                    }
                    return match op {
                        BinaryOp::Add => Ok((
                            format!("klassic_rt_str_concat({left}, {right})"),
                            CType::Str,
                        )),
                        BinaryOp::Equal => {
                            Ok((format!("klassic_rt_str_eq({left}, {right})"), CType::Bool))
                        }
                        BinaryOp::NotEqual => Ok((
                            format!("(!klassic_rt_str_eq({left}, {right}))"),
                            CType::Bool,
                        )),
                        _ => Err(unsupported(*span, "this string operator")),
                    };
                }
                if left_ty != right_ty {
                    return Err(unsupported(*span, "mixed numeric operand types"));
                }
                let ty = match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        left_ty
                    }
                    _ => CType::Bool,
                };
                Ok((format!("({left} {c_op} {right})"), ty))
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let Some(else_branch) = else_branch else {
                    return Err(unsupported(*span, "if without else as an expression"));
                };
                let (cond, _) = self.expr(condition)?;
                let (then_part, then_ty) = self.expr(then_branch)?;
                let (else_part, else_ty) = self.expr(else_branch)?;
                if then_ty != else_ty {
                    return Err(unsupported(*span, "if branches with different types"));
                }
                Ok((format!("({cond} ? {then_part} : {else_part})"), then_ty))
            }
            Expr::Call {
                callee,
                arguments,
                span,
            } => {
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return Err(unsupported(*span, "calling a non-identifier"));
                };
                if let Some(result) = self.builtin_call(name, arguments, *span)? {
                    return Ok(result);
                }
                let Some(function) = self.function(name) else {
                    return Err(unsupported(*span, &format!("function `{name}`")));
                };
                let (params, ret) = (function.params.clone(), function.ret);
                if params.len() != arguments.len() {
                    return Err(Diagnostic::compile(
                        *span,
                        format!(
                            "{name} expects {} arguments but got {}",
                            params.len(),
                            arguments.len()
                        ),
                    ));
                }
                let mut rendered = Vec::with_capacity(arguments.len());
                for (argument, (_, expected)) in arguments.iter().zip(params.iter()) {
                    let (code, ty) = self.expr(argument)?;
                    if ty != *expected {
                        return Err(unsupported(argument.span(), "argument of this type"));
                    }
                    rendered.push(code);
                }
                Ok((format!("{}({})", c_ident(name), rendered.join(", ")), ret))
            }
            other => Err(unsupported(other.span(), "this expression")),
        }
    }

    /// String / display builtins served by the `klassic_rt_*` shims.
    /// Returns Ok(None) when `name` is not a builtin (a user function).
    fn builtin_call(
        &mut self,
        name: &str,
        arguments: &[Expr],
        span: Span,
    ) -> Result<Option<(String, CType)>, Diagnostic> {
        match (name, arguments.len()) {
            ("length", 1) => {
                let (s, ty) = self.expr(&arguments[0])?;
                if ty != CType::Str {
                    return Err(unsupported(span, "length of a non-string"));
                }
                Ok(Some((format!("klassic_rt_str_len_chars({s})"), CType::Int)))
            }
            ("isEmptyString", 1) => {
                let (s, ty) = self.expr(&arguments[0])?;
                if ty != CType::Str {
                    return Err(unsupported(span, "isEmptyString of a non-string"));
                }
                Ok(Some((
                    format!("(klassic_rt_str_len_chars({s}) == 0)"),
                    CType::Bool,
                )))
            }
            ("substring", 3) => {
                let (s, ty) = self.expr(&arguments[0])?;
                let (start, start_ty) = self.expr(&arguments[1])?;
                let (end, end_ty) = self.expr(&arguments[2])?;
                if ty != CType::Str || start_ty != CType::Int || end_ty != CType::Int {
                    return Err(unsupported(span, "substring with these argument types"));
                }
                Ok(Some((
                    format!("klassic_rt_str_substring({s}, {start}, {end})"),
                    CType::Str,
                )))
            }
            ("at", 2) => {
                let (s, ty) = self.expr(&arguments[0])?;
                let (index, index_ty) = self.expr(&arguments[1])?;
                if ty != CType::Str || index_ty != CType::Int {
                    return Err(unsupported(span, "at with these argument types"));
                }
                Ok(Some((
                    format!("klassic_rt_str_at({s}, {index})"),
                    CType::Str,
                )))
            }
            ("toString", 1) => {
                let (value, ty) = self.expr(&arguments[0])?;
                let code = match ty {
                    CType::Int => format!("klassic_rt_i64_to_str({value})"),
                    CType::Double => format!("klassic_rt_f64_to_str({value})"),
                    CType::Bool => format!("klassic_rt_bool_to_str({value})"),
                    CType::Str => value,
                    CType::Unit => return Err(unsupported(span, "toString of unit")),
                };
                Ok(Some((code, CType::Str)))
            }
            _ => Ok(None),
        }
    }

    /// Emit `expr` as a C statement (or sequence of statements).
    fn statement(&mut self, expr: &Expr) -> Result<(), Diagnostic> {
        match expr {
            Expr::Block { expressions, .. } => {
                self.line("{");
                self.indent += 1;
                self.locals.push(Vec::new());
                for sub in expressions {
                    self.statement(sub)?;
                }
                self.locals.pop();
                self.indent -= 1;
                self.line("}");
                Ok(())
            }
            Expr::VarDecl { name, value, .. } => {
                let (code, ty) = self.expr(value)?;
                self.line(&format!("{} {} = {};", ty.c_name(), c_ident(name), code));
                self.locals
                    .last_mut()
                    .expect("c emitter scope")
                    .push((name.clone(), ty));
                Ok(())
            }
            Expr::Assign { name, value, span } => {
                let Some(expected) = self.lookup(name) else {
                    return Err(unsupported(*span, &format!("assignment to `{name}`")));
                };
                let (code, ty) = self.expr(value)?;
                if ty != expected {
                    return Err(unsupported(*span, "assignment changing a type"));
                }
                self.line(&format!("{} = {};", c_ident(name), code));
                Ok(())
            }
            Expr::While {
                condition, body, ..
            } => {
                let (cond, _) = self.expr(condition)?;
                self.line(&format!("while ({cond})"));
                self.statement(body)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let (cond, _) = self.expr(condition)?;
                self.line(&format!("if ({cond})"));
                self.statement(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.line("else");
                    self.statement(else_branch)?;
                }
                Ok(())
            }
            Expr::Call {
                callee, arguments, ..
            } if matches!(callee.as_ref(), Expr::Identifier { name, .. } if name == "println") => {
                if arguments.len() != 1 {
                    return Err(Diagnostic::compile(
                        expr.span(),
                        format!("println expects 1 argument but got {}", arguments.len()),
                    ));
                }
                let (code, ty) = self.expr(&arguments[0])?;
                match ty {
                    CType::Int => self.line(&format!("klassic_rt_println_i64({code});")),
                    CType::Double => self.line(&format!("klassic_rt_println_f64({code});")),
                    CType::Bool => self.line(&format!("klassic_rt_println_bool({code});")),
                    CType::Str => self.line(&format!("klassic_rt_println_str({code});")),
                    CType::Unit => return Err(unsupported(expr.span(), "printing unit")),
                }
                Ok(())
            }
            Expr::ModuleHeader { .. } | Expr::Import { .. } | Expr::DefDecl { .. } => Ok(()),
            other => {
                // An expression in statement position: evaluate for
                // effect (the subset has no effectful expressions other
                // than calls, but discarding a pure value is harmless).
                let (code, _) = self.expr(other)?;
                self.line(&format!("(void)({code});"));
                Ok(())
            }
        }
    }
}

fn collect_functions(expr: &Expr, emitter: &mut CEmitter) -> Result<(), Diagnostic> {
    let Expr::Block { expressions, .. } = expr else {
        return Ok(());
    };
    for expression in expressions {
        let Expr::DefDecl {
            name,
            params,
            param_annotations,
            return_annotation,
            body,
            span,
            ..
        } = expression
        else {
            continue;
        };
        let mut c_params = Vec::with_capacity(params.len());
        for (param, annotation) in params.iter().zip(param_annotations.iter()) {
            let Some(annotation) = annotation else {
                return Err(unsupported(*span, "an unannotated function parameter"));
            };
            c_params.push((
                param.clone(),
                ctype_from_annotation(&annotation.text, *span)?,
            ));
        }
        let Some(return_annotation) = return_annotation else {
            return Err(unsupported(*span, "a function without a return annotation"));
        };
        let ret = ctype_from_annotation(&return_annotation.text, *span)?;
        emitter.functions.push(CFunction {
            name: name.clone(),
            params: c_params,
            ret,
            body: body.as_ref().clone(),
        });
    }
    Ok(())
}

/// Emit the whole program as one C translation unit.
pub(crate) fn emit_c_program(expr: &Expr) -> Result<String, Diagnostic> {
    let mut emitter = CEmitter::default();
    collect_functions(expr, &mut emitter)?;

    let mut out = String::new();
    out.push_str("/* generated by klassic --backend c */\n");
    out.push_str("#include <stdint.h>\n#include <stddef.h>\n\n");
    out.push_str(concat!(
        "/* klassic runtime ABI — link libklassic_runtime.a */\n",
        "typedef struct { const uint8_t* ptr; size_t len; } KStr;\n",
        "#define KL_LIT(s, n) ((KStr){ (const uint8_t*)(s), (size_t)(n) })\n",
        "extern KStr klassic_rt_str_concat(KStr a, KStr b);\n",
        "extern int64_t klassic_rt_str_eq(KStr a, KStr b);\n",
        "extern int64_t klassic_rt_str_len_chars(KStr s);\n",
        "extern KStr klassic_rt_str_substring(KStr s, int64_t start, int64_t end);\n",
        "extern KStr klassic_rt_str_at(KStr s, int64_t index);\n",
        "extern KStr klassic_rt_i64_to_str(int64_t value);\n",
        "extern KStr klassic_rt_bool_to_str(int64_t value);\n",
        "extern KStr klassic_rt_f64_to_str(double value);\n",
        "extern void klassic_rt_print_str(KStr s);\n",
        "extern void klassic_rt_println_str(KStr s);\n",
        "extern void klassic_rt_println_i64(int64_t value);\n",
        "extern void klassic_rt_println_f64(double value);\n",
        "extern void klassic_rt_println_bool(int64_t value);\n",
        "\n",
    ));

    // Prototypes first so functions can call each other and recurse.
    for function in &emitter.functions {
        let params = function
            .params
            .iter()
            .map(|(name, ty)| format!("{} {}", ty.c_name(), c_ident(name)))
            .collect::<Vec<_>>()
            .join(", ");
        out.push_str(&format!(
            "{} {}({});\n",
            function.ret.c_name(),
            c_ident(&function.name),
            params
        ));
    }
    out.push('\n');

    // Function bodies: the subset treats a function body as a single
    // expression returned to the caller.
    for index in 0..emitter.functions.len() {
        let function = &emitter.functions[index];
        let (name, params, ret, body) = (
            function.name.clone(),
            function.params.clone(),
            function.ret,
            function.body.clone(),
        );
        let rendered_params = params
            .iter()
            .map(|(param, ty)| format!("{} {}", ty.c_name(), c_ident(param)))
            .collect::<Vec<_>>()
            .join(", ");
        emitter.locals.push(params.clone());
        let (code, body_ty) = emitter.expr(&body)?;
        emitter.locals.pop();
        if body_ty != ret {
            return Err(unsupported(
                body.span(),
                "a function body of a different type than its return annotation",
            ));
        }
        out.push_str(&format!(
            "{} {}({}) {{\n    return {};\n}}\n\n",
            ret.c_name(),
            c_ident(&name),
            rendered_params,
            code
        ));
    }

    // Main: every non-def top-level statement in order.
    emitter.locals.push(Vec::new());
    emitter.indent = 1;
    if let Expr::Block { expressions, .. } = expr {
        for expression in expressions {
            emitter.statement(expression)?;
        }
    } else {
        emitter.statement(expr)?;
    }
    let body = std::mem::take(&mut emitter.body);
    out.push_str("int main(void) {\n");
    out.push_str(&body);
    out.push_str("    return 0;\n}\n");
    Ok(out)
}
