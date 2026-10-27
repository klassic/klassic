//! LLVM mutator lowering for the shared offset-reference ABI. Every heap
//! operand is a stable RootCell pointer. Raw managed addresses never enter IR.
use super::*;

impl Emitter {
    pub(super) fn gen_promote_numeric(&mut self, value: Value, promoted: LType) -> Value {
        if value.ty == promoted {
            return value;
        }
        let operation = if value.ty == LType::I64 {
            "sitofp"
        } else {
            "fpext"
        };
        let operand = self.fresh();
        self.emit(&format!(
            "{operand} = {operation} {} {} to {}",
            value.ty.llvm(),
            value.operand,
            promoted.llvm()
        ));
        Value {
            operand,
            ty: promoted,
        }
    }
    pub(super) fn gen_error_prefix(&mut self, span: Span) -> (String, usize) {
        let prefix = self
            .gen_source
            .as_ref()
            .expect("canonical source context")
            .prefix(span, self.gen_current_function.clone());
        let name = format!("@kg_error_{}", self.gen_next_error);
        self.gen_next_error += 1;
        let bytes = prefix
            .as_bytes()
            .iter()
            .copied()
            .chain(std::iter::once(0))
            .map(|b| format!("\\{b:02X}"))
            .collect::<String>();
        self.gen_string_globals.push_str(&format!(
            "{name} = private unnamed_addr constant [{} x i8] c\"{bytes}\"\n",
            prefix.len() + 1
        ));
        (name, prefix.len())
    }
    pub(super) fn gen_snapshot(&mut self, value: Value) -> Value {
        let root = self.gen_root();
        self.emit(&format!(
            "call void @kg_root_copy(ptr {root}, ptr {})",
            value.operand
        ));
        Value {
            operand: root,
            ty: value.ty,
        }
    }
    pub(super) fn gen_string(&mut self, value: &str) -> Value {
        let name = format!("@kg_literal_{}", self.gen_next_string);
        self.gen_next_string += 1;
        let bytes = value
            .as_bytes()
            .iter()
            .map(|b| format!("\\{b:02X}"))
            .collect::<String>();
        self.gen_string_globals.push_str(&format!(
            "{name} = private unnamed_addr constant [{} x i8] c\"{bytes}\"\n",
            value.len()
        ));
        let root = self.gen_root();
        self.emit(&format!(
            "call void @kg_string_from_utf8(ptr {name}, i64 {}, ptr {root})",
            value.len()
        ));
        Value {
            operand: root,
            ty: LType::String,
        }
    }
    pub(super) fn gen_string_binary(
        &mut self,
        left: Value,
        op: BinaryOp,
        right: Value,
        span: Span,
    ) -> Result<Value, Diagnostic> {
        if op == BinaryOp::Add {
            let root = self.gen_root();
            self.emit(&format!(
                "call void @kg_string_concat(ptr {}, ptr {}, ptr {root})",
                left.operand, right.operand
            ));
            return Ok(Value {
                operand: root,
                ty: LType::String,
            });
        }
        let predicate = match op {
            BinaryOp::Equal => "eq",
            BinaryOp::NotEqual => "ne",
            BinaryOp::Less => "slt",
            BinaryOp::LessEqual => "sle",
            BinaryOp::Greater => "sgt",
            BinaryOp::GreaterEqual => "sge",
            _ => return Err(unsupported(span, "this String operator")),
        };
        let cmp = self.fresh();
        self.emit(&format!(
            "{cmp} = call i64 @kg_string_compare(ptr {}, ptr {})",
            left.operand, right.operand
        ));
        let out = self.fresh();
        self.emit(&format!("{out} = icmp {predicate} i64 {cmp}, 0"));
        Ok(Value {
            operand: out,
            ty: LType::I1,
        })
    }
    pub(super) fn gen_logical(
        &mut self,
        lhs: &Expr,
        op: BinaryOp,
        rhs: &Expr,
        span: Span,
    ) -> Result<Value, Diagnostic> {
        let left = self.expr(lhs)?;
        if left.ty != LType::I1 {
            return Err(unsupported(span, "a non-Boolean logical operand"));
        }
        let before = self.current_block.clone();
        let next = self.fresh_label("logicrhs");
        let merge = self.fresh_label("logicmerge");
        let (yes, no, skipped) = if op == BinaryOp::LogicalAnd {
            (&next, &merge, "false")
        } else {
            (&merge, &next, "true")
        };
        self.emit(&format!(
            "br i1 {}, label %{yes}, label %{no}",
            left.operand
        ));
        self.bind(&next);
        let right = self.expr(rhs)?;
        if right.ty != LType::I1 {
            return Err(unsupported(span, "a non-Boolean logical operand"));
        }
        let after = self.current_block.clone();
        self.emit(&format!("br label %{merge}"));
        self.bind(&merge);
        let operand = self.fresh();
        self.emit(&format!(
            "{operand} = phi i1 [ {skipped}, %{before} ], [ {}, %{after} ]",
            right.operand
        ));
        Ok(Value {
            operand,
            ty: LType::I1,
        })
    }
    pub(super) fn gen_reset(&mut self) {
        self.gen_current_function = None;
        self.gen_prologue.clear();
        self.gen_all_roots.clear();
        self.gen_scope_roots.clear();
    }
    /// Reserve a static root site at function entry. Loop executions reuse it;
    /// branch-only roots still dominate joins and start as empty cells.
    pub(super) fn gen_root(&mut self) -> String {
        let root = self.fresh();
        self.gen_prologue
            .push_str(&format!("  {root} = call ptr @kg_root_new()\n"));
        self.gen_all_roots.push(root.clone());
        self.gen_scope_roots
            .last_mut()
            .expect("root scope")
            .push(root.clone());
        root
    }
    pub(super) fn gen_escape(&mut self, value: Value) -> Value {
        let root = self.gen_root();
        self.gen_scope_roots.last_mut().unwrap().pop();
        let parent = self.gen_scope_roots.len() - 2;
        self.gen_scope_roots[parent].push(root.clone());
        self.emit(&format!(
            "call void @kg_root_copy(ptr {root}, ptr {})",
            value.operand
        ));
        Value {
            operand: root,
            ty: value.ty,
        }
    }
    pub(super) fn gen_release_frame(&mut self) {
        if !self.generational {
            return;
        }
        for root in self.gen_all_roots.clone().into_iter().rev() {
            self.emit(&format!("call void @kg_root_release(ptr {root})"));
        }
    }
    pub(super) fn gen_record_literal(
        &mut self,
        fields: &[(String, Expr)],
        span: Span,
    ) -> Result<Value, Diagnostic> {
        if fields.is_empty() {
            return Err(unsupported(span, "an empty record"));
        }
        let record = self.gen_root();
        self.emit(&format!("call void @kg_root_clear(ptr {record})"));
        let mut leaves = Vec::new();
        let mut shape = Vec::new();
        for (name, expression) in fields {
            let value = self.expr(expression)?;
            let leaf = if value.ty.is_heap() {
                self.gen_snapshot(Value {
                    operand: value.operand,
                    ty: value.ty,
                })
                .operand
            } else {
                let bits = match value.ty {
                    LType::I64 => value.operand,
                    LType::F32 => {
                        let narrow = self.fresh();
                        self.emit(&format!(
                            "{narrow} = bitcast float {} to i32",
                            value.operand
                        ));
                        let bits = self.fresh();
                        self.emit(&format!("{bits} = zext i32 {narrow} to i64"));
                        bits
                    }
                    LType::F64 => {
                        let bits = self.fresh();
                        self.emit(&format!("{bits} = bitcast double {} to i64", value.operand));
                        bits
                    }
                    LType::I1 => {
                        let bits = self.fresh();
                        self.emit(&format!("{bits} = zext i1 {} to i64", value.operand));
                        bits
                    }
                    LType::Unit => {
                        return Err(unsupported(expression.span(), "a unit record field"));
                    }
                    _ => unreachable!("managed fields handled above"),
                };
                let leaf = self.gen_root();
                self.emit(&format!(
                    "call void @kg_alloc_into(i64 8, i64 1, ptr {leaf})"
                ));
                self.emit(&format!(
                    "call void @kg_store_i64(ptr {leaf}, i64 0, i64 {bits})"
                ));
                leaf
            };
            leaves.push(leaf);
            shape.push((name.clone(), value.ty));
        }
        self.emit(&format!(
            "call void @kg_alloc_into(i64 {}, i64 2, ptr {record})",
            fields.len() * 8
        ));
        for (index, leaf) in leaves.into_iter().enumerate() {
            self.emit(&format!(
                "call void @kg_store_ref(ptr {record}, i64 {}, ptr {leaf})",
                index * 8
            ));
            self.emit(&format!("call void @kg_root_clear(ptr {leaf})"));
        }
        let shape_id = self.intern_shape(shape);
        Ok(Value {
            operand: record,
            ty: LType::Record(shape_id),
        })
    }
    pub(super) fn gen_field_access(
        &mut self,
        target: &Expr,
        field: &str,
        span: Span,
    ) -> Result<Value, Diagnostic> {
        let value = self.expr(target)?;
        let LType::Record(shape_id) = value.ty else {
            return Err(unsupported(span, "a field access on a non-record value"));
        };
        let Some((index, (_, ty))) = self.shapes[shape_id]
            .iter()
            .enumerate()
            .find(|(_, (name, _))| name == field)
        else {
            return Err(unsupported(
                span,
                &format!("no field `{field}` on this record"),
            ));
        };
        let ty = *ty;
        let leaf = self.gen_root();
        self.emit(&format!(
            "call void @kg_load_ref_into(ptr {}, i64 {}, ptr {leaf})",
            value.operand,
            index * 8
        ));
        if ty.is_heap() {
            return Ok(Value { operand: leaf, ty });
        }
        let bits = self.fresh();
        self.emit(&format!(
            "{bits} = call i64 @kg_load_i64(ptr {leaf}, i64 0)"
        ));
        self.emit(&format!("call void @kg_root_clear(ptr {leaf})"));
        let operand = match ty {
            LType::I64 => bits,
            LType::F32 => {
                let narrow = self.fresh();
                self.emit(&format!("{narrow} = trunc i64 {bits} to i32"));
                let result = self.fresh();
                self.emit(&format!("{result} = bitcast i32 {narrow} to float"));
                result
            }
            LType::F64 => {
                let result = self.fresh();
                self.emit(&format!("{result} = bitcast i64 {bits} to double"));
                result
            }
            LType::I1 => {
                let result = self.fresh();
                self.emit(&format!("{result} = trunc i64 {bits} to i1"));
                result
            }
            _ => return Err(unsupported(span, "a field of an unsupported type")),
        };
        Ok(Value { operand, ty })
    }
}

pub(crate) fn emit_llvm_generational_program(
    expr: &Expr,
    config: crate::GenerationalBackendConfig,
    source: &crate::generational_modules::Sources,
    function_names: &std::collections::HashMap<String, String>,
) -> Result<String, Diagnostic> {
    if config.flags & !7 != 0 {
        return Err(unsupported(
            expr.span(),
            "unknown canonical GC configuration flags",
        ));
    }
    let mut emitter = Emitter {
        generational: true,
        gen_config: config,
        gen_source: Some(source.clone()),
        gen_function_names: function_names.clone(),
        ..Emitter::default()
    };
    collect_functions(expr, &mut emitter)?;
    let code = emit_functions_and_main(&mut emitter, expr)?;
    let mut out = String::from(
        "; generated by klassic: canonical generational RootCell ABI\ntarget triple = \"x86_64-pc-linux-gnu\"\n",
    );
    out.push_str("declare void @klassic_rt_println_i64(i64)\ndeclare void @klassic_rt_println_f64(double)\ndeclare void @klassic_rt_println_f32(float)\ndeclare void @klassic_rt_println_bool(i64)\n");
    out.push_str("declare void @kg_configure(i64, i64, i64, i64)\ndeclare void @kg_configure_uncommit_delay(i64)\ndeclare void @kg_init()\ndeclare void @kg_shutdown()\ndeclare void @kg_poll()\ndeclare void @kg_native_enter()\ndeclare void @kg_native_exit()\ndeclare ptr @kg_root_new()\ndeclare void @kg_root_copy(ptr, ptr)\ndeclare void @kg_root_clear(ptr)\ndeclare void @kg_root_release(ptr)\ndeclare void @kg_alloc_into(i64, i64, ptr)\ndeclare void @kg_store_ref(ptr, i64, ptr)\ndeclare void @kg_load_ref_into(ptr, i64, ptr)\ndeclare void @kg_store_i64(ptr, i64, i64)\ndeclare i64 @kg_load_i64(ptr, i64)\n\n");
    out.push_str("declare void @kg_string_from_utf8(ptr, i64, ptr)\ndeclare void @kg_string_concat(ptr, ptr, ptr)\ndeclare i64 @kg_string_length(ptr)\ndeclare i64 @kg_string_compare(ptr, ptr)\ndeclare void @kg_string_println(ptr)\n");
    out.push_str("declare i64 @kg_i64_add(i64, i64)\ndeclare i64 @kg_i64_sub(i64, i64)\ndeclare i64 @kg_i64_mul(i64, i64)\ndeclare i64 @kg_i64_div(i64, i64)\ndeclare i64 @kg_i64_neg(i64)\n");
    out.push_str("declare i64 @kg_i64_add_at(i64, i64, ptr, i64)\ndeclare i64 @kg_i64_sub_at(i64, i64, ptr, i64)\ndeclare i64 @kg_i64_mul_at(i64, i64, ptr, i64)\ndeclare i64 @kg_i64_div_at(i64, i64, ptr, i64)\ndeclare i64 @kg_i64_neg_at(i64, ptr, i64)\n");
    out.push_str(&emitter.gen_string_globals);
    out.push('\n');
    out.push_str(&code);
    Ok(out)
}
