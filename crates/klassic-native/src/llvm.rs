//! LLVM backend (migration plan `docs/llvm-backend-plan.md`). Emits a
//! textual LLVM IR module that `clang` compiles and links against
//! `libklassic_runtime.a`. Coverage grows milestone by milestone; the
//! hand-emitted backend stays the default and untouched, and anything
//! outside the current subset fails with a source-located diagnostic --
//! never wrong code.
//!
//! M3 (scalar core): `Int` / `Double` / `Bool` / `Unit`, arithmetic /
//! comparison / logical operators, `val` / `mutable` / assignment,
//! `if` / `while`, `println` of a scalar, and annotated top-level `def`s
//! (including recursion). Locals become `alloca`s that clang's mem2reg
//! promotes to SSA; control flow lowers to basic blocks. Strings, the
//! heap, enums, closures, and the stdlib are later milestones.

use klassic_span::{Diagnostic, Span};
use klassic_syntax::{BinaryOp, Expr, UnaryOp};

fn unsupported(span: Span, feature: &str) -> Diagnostic {
    Diagnostic::compile(
        span,
        format!("{feature} is not supported by the LLVM backend yet"),
    )
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum LType {
    I64,
    F64,
    I1,
    Unit,
}

impl LType {
    fn llvm(self) -> &'static str {
        match self {
            LType::I64 => "i64",
            LType::F64 => "double",
            LType::I1 => "i1",
            LType::Unit => "void",
        }
    }
}

fn ltype_from_annotation(text: &str, span: Span) -> Result<LType, Diagnostic> {
    match text.trim() {
        "Int" | "Long" | "Short" | "Byte" => Ok(LType::I64),
        "Double" | "Float" => Ok(LType::F64),
        "Bool" | "Boolean" => Ok(LType::I1),
        "Unit" => Ok(LType::Unit),
        other => Err(unsupported(span, &format!("type annotation `{other}`"))),
    }
}

struct Function {
    name: String,
    params: Vec<(String, LType)>,
    ret: LType,
    body: Expr,
}

/// A lowered value: the SSA operand text and its type.
struct Value {
    operand: String,
    ty: LType,
}

#[derive(Default)]
struct Emitter {
    functions: Vec<Function>,
    /// entry-block `alloca`s for the function currently being emitted.
    allocas: String,
    /// instructions after the entry allocas (may contain labelled blocks).
    body: String,
    next_temp: u32,
    next_label: u32,
    /// The label of the block instructions are currently appended to;
    /// used to compute correct phi predecessors across nested control
    /// flow.
    current_block: String,
    /// lexical scopes: name -> (alloca pointer operand, type).
    scopes: Vec<Vec<(String, String, LType)>>,
}

impl Emitter {
    fn fresh(&mut self) -> String {
        let temp = format!("%t{}", self.next_temp);
        self.next_temp += 1;
        temp
    }

    fn fresh_label(&mut self, hint: &str) -> String {
        let label = format!("{hint}{}", self.next_label);
        self.next_label += 1;
        label
    }

    fn emit(&mut self, line: &str) {
        self.body.push_str("  ");
        self.body.push_str(line);
        self.body.push('\n');
    }

    fn bind(&mut self, label: &str) {
        self.body.push_str(label);
        self.body.push_str(":\n");
        self.current_block = label.to_owned();
    }

    /// Declare a local, allocating stack space (promoted to SSA by
    /// mem2reg) and recording it in the innermost scope. Returns the
    /// pointer operand.
    fn declare_local(&mut self, name: &str, ty: LType) -> String {
        let ptr = format!("%v{}", self.next_temp);
        self.next_temp += 1;
        self.allocas
            .push_str(&format!("  {ptr} = alloca {}\n", ty.llvm()));
        self.scopes
            .last_mut()
            .expect("a scope is open")
            .push((name.to_owned(), ptr.clone(), ty));
        ptr
    }

    fn lookup(&self, name: &str) -> Option<(String, LType)> {
        for scope in self.scopes.iter().rev() {
            for (n, ptr, ty) in scope.iter().rev() {
                if n == name {
                    return Some((ptr.clone(), *ty));
                }
            }
        }
        None
    }

    /// Lower an expression to a value, appending any instructions.
    fn expr(&mut self, expr: &Expr) -> Result<Value, Diagnostic> {
        match expr {
            Expr::Int { value, .. } => Ok(Value {
                operand: value.to_string(),
                ty: LType::I64,
            }),
            Expr::Double { value, .. } => Ok(Value {
                // Exact bit pattern -- LLVM accepts a hex double literal.
                operand: format!("0x{:016X}", value.to_bits()),
                ty: LType::F64,
            }),
            Expr::Bool { value, .. } => Ok(Value {
                operand: if *value { "true" } else { "false" }.to_owned(),
                ty: LType::I1,
            }),
            Expr::Identifier { name, span } => {
                let (ptr, ty) = self
                    .lookup(name)
                    .ok_or_else(|| unsupported(*span, &format!("identifier `{name}`")))?;
                let temp = self.fresh();
                self.emit(&format!("{temp} = load {}, ptr {ptr}", ty.llvm()));
                Ok(Value { operand: temp, ty })
            }
            Expr::Unary { op, expr, span } => {
                let value = self.expr(expr)?;
                match (op, value.ty) {
                    (UnaryOp::Plus, LType::I64 | LType::F64) => Ok(value),
                    (UnaryOp::Minus, LType::I64) => {
                        let temp = self.fresh();
                        self.emit(&format!("{temp} = sub i64 0, {}", value.operand));
                        Ok(Value {
                            operand: temp,
                            ty: LType::I64,
                        })
                    }
                    (UnaryOp::Minus, LType::F64) => {
                        let temp = self.fresh();
                        self.emit(&format!("{temp} = fneg double {}", value.operand));
                        Ok(Value {
                            operand: temp,
                            ty: LType::F64,
                        })
                    }
                    (UnaryOp::Not, LType::I1) => {
                        let temp = self.fresh();
                        self.emit(&format!("{temp} = xor i1 {}, true", value.operand));
                        Ok(Value {
                            operand: temp,
                            ty: LType::I1,
                        })
                    }
                    _ => Err(unsupported(*span, "this unary operator on this type")),
                }
            }
            Expr::Binary { lhs, op, rhs, span } => self.binary(lhs, *op, rhs, *span),
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let Some(else_branch) = else_branch else {
                    return Err(unsupported(*span, "an `if` expression without `else`"));
                };
                self.if_expr(condition, then_branch, else_branch, *span)
            }
            Expr::Block { expressions, span } => {
                // A block used as an expression: run all but the last for
                // effect, yield the last. Empty blocks have no value.
                let Some((last, leading)) = expressions.split_last() else {
                    return Err(unsupported(*span, "an empty block expression"));
                };
                self.scopes.push(Vec::new());
                for statement in leading {
                    self.statement(statement)?;
                }
                let value = self.expr(last)?;
                self.scopes.pop();
                Ok(value)
            }
            Expr::Call {
                callee,
                arguments,
                span,
            } => self.call(callee, arguments, *span),
            other => Err(unsupported(other.span(), "expression")),
        }
    }

    /// Lower a call to a user-defined top-level function.
    fn call(&mut self, callee: &Expr, arguments: &[Expr], span: Span) -> Result<Value, Diagnostic> {
        let Expr::Identifier { name, .. } = callee else {
            return Err(unsupported(
                span,
                "a call whose callee is not a plain function name",
            ));
        };
        // Copy the signature so args can be lowered (borrows &mut self).
        let Some((params, ret)) = self
            .functions
            .iter()
            .find(|function| &function.name == name)
            .map(|function| (function.params.clone(), function.ret))
        else {
            return Err(unsupported(span, &format!("call to `{name}`")));
        };
        if arguments.len() != params.len() {
            return Err(unsupported(
                span,
                &format!(
                    "call to `{name}` with {} arguments (expects {})",
                    arguments.len(),
                    params.len()
                ),
            ));
        }
        let mut argument_operands = Vec::with_capacity(arguments.len());
        for (argument, (_, param_ty)) in arguments.iter().zip(params.iter()) {
            let value = self.expr(argument)?;
            if value.ty != *param_ty {
                return Err(unsupported(span, &format!("argument type to `{name}`")));
            }
            argument_operands.push(format!("{} {}", param_ty.llvm(), value.operand));
        }
        let call = format!("@klassic_{name}({})", argument_operands.join(", "));
        if ret == LType::Unit {
            self.emit(&format!("call void {call}"));
            Ok(Value {
                operand: String::new(),
                ty: LType::Unit,
            })
        } else {
            let temp = self.fresh();
            self.emit(&format!("{temp} = call {} {call}", ret.llvm()));
            Ok(Value {
                operand: temp,
                ty: ret,
            })
        }
    }

    fn binary(
        &mut self,
        lhs: &Expr,
        op: BinaryOp,
        rhs: &Expr,
        span: Span,
    ) -> Result<Value, Diagnostic> {
        let left = self.expr(lhs)?;
        let right = self.expr(rhs)?;
        if left.ty != right.ty {
            return Err(unsupported(span, "a binary operator on mixed types"));
        }
        let temp = self.fresh();
        // Arithmetic keeps the operand type; comparisons yield i1; logical
        // operators need i1 operands.
        let (instr, result_ty) = match (op, left.ty) {
            (BinaryOp::Add, LType::I64) => ("add i64", LType::I64),
            (BinaryOp::Subtract, LType::I64) => ("sub i64", LType::I64),
            (BinaryOp::Multiply, LType::I64) => ("mul i64", LType::I64),
            (BinaryOp::Divide, LType::I64) => ("sdiv i64", LType::I64),
            (BinaryOp::Add, LType::F64) => ("fadd double", LType::F64),
            (BinaryOp::Subtract, LType::F64) => ("fsub double", LType::F64),
            (BinaryOp::Multiply, LType::F64) => ("fmul double", LType::F64),
            (BinaryOp::Divide, LType::F64) => ("fdiv double", LType::F64),
            (BinaryOp::Less, LType::I64) => ("icmp slt i64", LType::I1),
            (BinaryOp::LessEqual, LType::I64) => ("icmp sle i64", LType::I1),
            (BinaryOp::Greater, LType::I64) => ("icmp sgt i64", LType::I1),
            (BinaryOp::GreaterEqual, LType::I64) => ("icmp sge i64", LType::I1),
            (BinaryOp::Equal, LType::I64) => ("icmp eq i64", LType::I1),
            (BinaryOp::NotEqual, LType::I64) => ("icmp ne i64", LType::I1),
            (BinaryOp::Less, LType::F64) => ("fcmp olt double", LType::I1),
            (BinaryOp::LessEqual, LType::F64) => ("fcmp ole double", LType::I1),
            (BinaryOp::Greater, LType::F64) => ("fcmp ogt double", LType::I1),
            (BinaryOp::GreaterEqual, LType::F64) => ("fcmp oge double", LType::I1),
            (BinaryOp::Equal, LType::F64) => ("fcmp oeq double", LType::I1),
            (BinaryOp::NotEqual, LType::F64) => ("fcmp one double", LType::I1),
            (BinaryOp::Equal, LType::I1) => ("icmp eq i1", LType::I1),
            (BinaryOp::NotEqual, LType::I1) => ("icmp ne i1", LType::I1),
            (BinaryOp::LogicalAnd, LType::I1) => ("and i1", LType::I1),
            (BinaryOp::LogicalOr, LType::I1) => ("or i1", LType::I1),
            _ => return Err(unsupported(span, "this binary operator on this type")),
        };
        self.emit(&format!(
            "{temp} = {instr} {}, {}",
            left.operand, right.operand
        ));
        Ok(Value {
            operand: temp,
            ty: result_ty,
        })
    }

    fn if_expr(
        &mut self,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: &Expr,
        span: Span,
    ) -> Result<Value, Diagnostic> {
        let cond = self.expr(condition)?;
        if cond.ty != LType::I1 {
            return Err(unsupported(span, "an `if` condition that is not Bool"));
        }
        let then_label = self.fresh_label("then");
        let else_label = self.fresh_label("else");
        let merge_label = self.fresh_label("ifcont");
        self.emit(&format!(
            "br i1 {}, label %{then_label}, label %{else_label}",
            cond.operand
        ));
        // then branch -- its value's actual predecessor block is wherever
        // control ends up after any nested control flow, not then_label.
        self.bind(&then_label);
        let then_value = self.expr(then_branch)?;
        let then_pred = self.current_block.clone();
        self.emit(&format!("br label %{merge_label}"));
        // else branch.
        self.bind(&else_label);
        let else_value = self.expr(else_branch)?;
        let else_pred = self.current_block.clone();
        if then_value.ty != else_value.ty {
            return Err(unsupported(
                span,
                "an `if` whose branches have different types",
            ));
        }
        self.emit(&format!("br label %{merge_label}"));
        self.bind(&merge_label);
        if then_value.ty == LType::Unit {
            return Ok(Value {
                operand: String::new(),
                ty: LType::Unit,
            });
        }
        let temp = self.fresh();
        self.emit(&format!(
            "{temp} = phi {} [ {}, %{then_pred} ], [ {}, %{else_pred} ]",
            then_value.ty.llvm(),
            then_value.operand,
            else_value.operand,
        ));
        Ok(Value {
            operand: temp,
            ty: then_value.ty,
        })
    }

    /// Lower a statement (value discarded).
    fn statement(&mut self, expr: &Expr) -> Result<(), Diagnostic> {
        match expr {
            Expr::Block { expressions, .. } => {
                self.scopes.push(Vec::new());
                for statement in expressions {
                    self.statement(statement)?;
                }
                self.scopes.pop();
                Ok(())
            }
            Expr::VarDecl { name, value, .. } => {
                let value = self.expr(value)?;
                let ptr = self.declare_local(name, value.ty);
                if value.ty != LType::Unit {
                    self.emit(&format!(
                        "store {} {}, ptr {ptr}",
                        value.ty.llvm(),
                        value.operand
                    ));
                }
                Ok(())
            }
            Expr::Assign { name, value, span } => {
                let (ptr, expected) = self
                    .lookup(name)
                    .ok_or_else(|| unsupported(*span, &format!("assignment to `{name}`")))?;
                let value = self.expr(value)?;
                if value.ty != expected {
                    return Err(unsupported(*span, "assignment changing a type"));
                }
                self.emit(&format!(
                    "store {} {}, ptr {ptr}",
                    value.ty.llvm(),
                    value.operand
                ));
                Ok(())
            }
            Expr::While {
                condition,
                body,
                span,
            } => {
                let cond_label = self.fresh_label("wcond");
                let body_label = self.fresh_label("wbody");
                let after_label = self.fresh_label("wafter");
                self.emit(&format!("br label %{cond_label}"));
                self.bind(&cond_label);
                let cond = self.expr(condition)?;
                if cond.ty != LType::I1 {
                    return Err(unsupported(*span, "a `while` condition that is not Bool"));
                }
                self.emit(&format!(
                    "br i1 {}, label %{body_label}, label %{after_label}",
                    cond.operand
                ));
                self.bind(&body_label);
                self.statement(body)?;
                self.emit(&format!("br label %{cond_label}"));
                self.bind(&after_label);
                Ok(())
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let cond = self.expr(condition)?;
                if cond.ty != LType::I1 {
                    return Err(unsupported(*span, "an `if` condition that is not Bool"));
                }
                let then_label = self.fresh_label("then");
                let after_label = self.fresh_label("ifafter");
                let else_label = if else_branch.is_some() {
                    self.fresh_label("else")
                } else {
                    after_label.clone()
                };
                self.emit(&format!(
                    "br i1 {}, label %{then_label}, label %{else_label}",
                    cond.operand
                ));
                self.bind(&then_label);
                self.statement(then_branch)?;
                self.emit(&format!("br label %{after_label}"));
                if let Some(else_branch) = else_branch {
                    self.bind(&else_label);
                    self.statement(else_branch)?;
                    self.emit(&format!("br label %{after_label}"));
                }
                self.bind(&after_label);
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
                let value = self.expr(&arguments[0])?;
                match value.ty {
                    LType::I64 => self.emit(&format!(
                        "call void @klassic_rt_println_i64(i64 {})",
                        value.operand
                    )),
                    LType::F64 => self.emit(&format!(
                        "call void @klassic_rt_println_f64(double {})",
                        value.operand
                    )),
                    LType::I1 => {
                        let widened = self.fresh();
                        self.emit(&format!("{widened} = zext i1 {} to i64", value.operand));
                        self.emit(&format!(
                            "call void @klassic_rt_println_bool(i64 {widened})"
                        ));
                    }
                    LType::Unit => return Err(unsupported(expr.span(), "printing unit")),
                }
                Ok(())
            }
            Expr::ModuleHeader { .. } | Expr::Import { .. } | Expr::DefDecl { .. } => Ok(()),
            other => {
                // An expression in statement position: evaluate for effect.
                self.expr(other)?;
                Ok(())
            }
        }
    }
}

fn collect_functions(expr: &Expr, emitter: &mut Emitter) -> Result<(), Diagnostic> {
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
        let mut lowered = Vec::with_capacity(params.len());
        for (param, annotation) in params.iter().zip(param_annotations.iter()) {
            let Some(annotation) = annotation else {
                return Err(unsupported(*span, "an unannotated function parameter"));
            };
            lowered.push((
                param.clone(),
                ltype_from_annotation(&annotation.text, *span)?,
            ));
        }
        let Some(return_annotation) = return_annotation else {
            return Err(unsupported(*span, "a function without a return annotation"));
        };
        let ret = ltype_from_annotation(&return_annotation.text, *span)?;
        emitter.functions.push(Function {
            name: name.clone(),
            params: lowered,
            ret,
            body: body.as_ref().clone(),
        });
    }
    Ok(())
}

/// Emit the whole program as an LLVM IR module.
pub(crate) fn emit_llvm_program(expr: &Expr) -> Result<String, Diagnostic> {
    let mut emitter = Emitter::default();
    collect_functions(expr, &mut emitter)?;

    let mut out = String::new();
    out.push_str("; generated by klassic --backend llvm\n");
    out.push_str("target triple = \"x86_64-pc-linux-gnu\"\n\n");
    out.push_str("declare void @klassic_rt_println_i64(i64)\n");
    out.push_str("declare void @klassic_rt_println_f64(double)\n");
    out.push_str("declare void @klassic_rt_println_bool(i64)\n\n");

    // User functions. The subset treats a function body as a single
    // expression returned to the caller.
    let function_count = emitter.functions.len();
    for index in 0..function_count {
        let (name, params, ret, body) = {
            let function = &emitter.functions[index];
            (
                function.name.clone(),
                function.params.clone(),
                function.ret,
                // clone the body so the emitter can borrow &mut self
                function.body.clone(),
            )
        };
        emitter.allocas.clear();
        emitter.body.clear();
        emitter.next_temp = 0;
        emitter.next_label = 0;
        emitter.scopes.clear();
        emitter.scopes.push(Vec::new());
        emitter.current_block = "entry".to_owned();
        // Bind parameters: alloca + store the incoming SSA value.
        let mut param_sig = Vec::with_capacity(params.len());
        for (param, ty) in &params {
            let incoming = format!("%arg_{param}");
            param_sig.push(format!("{} {incoming}", ty.llvm()));
            let ptr = emitter.declare_local(param, *ty);
            if *ty != LType::Unit {
                emitter
                    .body
                    .push_str(&format!("  store {} {incoming}, ptr {ptr}\n", ty.llvm()));
            }
        }
        let value = emitter.expr(&body)?;
        if value.ty != ret {
            return Err(unsupported(
                body.span(),
                "a function body of a different type than its return annotation",
            ));
        }
        let mangled = format!("klassic_{name}");
        out.push_str(&format!(
            "define {} @{mangled}({}) {{\nentry:\n",
            ret.llvm(),
            param_sig.join(", ")
        ));
        out.push_str(&emitter.allocas);
        out.push_str(&emitter.body);
        if ret == LType::Unit {
            out.push_str("  ret void\n");
        } else {
            out.push_str(&format!("  ret {} {}\n", ret.llvm(), value.operand));
        }
        out.push_str("}\n\n");
    }

    // main: every non-def top-level statement, in order.
    emitter.allocas.clear();
    emitter.body.clear();
    emitter.next_temp = 0;
    emitter.next_label = 0;
    emitter.scopes.clear();
    emitter.scopes.push(Vec::new());
    emitter.current_block = "entry".to_owned();
    let statements: Vec<&Expr> = match expr {
        Expr::Block { expressions, .. } => expressions.iter().collect(),
        other => vec![other],
    };
    for statement in statements {
        emitter.statement(statement)?;
    }
    out.push_str("define i32 @main() {\nentry:\n");
    out.push_str(&emitter.allocas);
    out.push_str(&emitter.body);
    out.push_str("  ret i32 0\n}\n");

    Ok(out)
}

#[cfg(test)]
mod tests {
    use crate::compile_source_to_llvm;

    #[test]
    fn println_of_an_integer_literal_lowers_to_a_runtime_call() {
        let ir = compile_source_to_llvm("t", "println(42)\n").expect("should compile");
        assert!(ir.contains("declare void @klassic_rt_println_i64(i64)"));
        assert!(ir.contains("define i32 @main()"));
        assert!(ir.contains("call void @klassic_rt_println_i64(i64 42)"));
    }

    #[test]
    fn integer_arithmetic_lowers_with_precedence() {
        let ir = compile_source_to_llvm("t", "println(1 + 2 * 3)\n").expect("should compile");
        assert!(ir.contains("mul i64 2, 3"), "IR was:\n{ir}");
        assert!(ir.contains("add i64 1, %t0"), "IR was:\n{ir}");
    }

    #[test]
    fn unsupported_construct_fails_cleanly() {
        // Strings are a later milestone; must be rejected, never miscompiled.
        assert!(compile_source_to_llvm("t", "println(\"hi\")\n").is_err());
    }
}
