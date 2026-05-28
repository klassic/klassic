use std::cell::RefCell;
use std::collections::HashMap;

use klassic_span::{Diagnostic, DiagnosticKind, Severity, Span};
use klassic_syntax::{
    Expr, FloatLiteralKind, IntLiteralKind, RecordField, TypeAnnotation, TypeClassConstraint,
    TypeClassMethod,
};

pub mod proof;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum GenericVar {
    Type(u32),
    Row(u32),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Bool,
    Prop,
    String,
    Unit,
    Dynamic,
    Null,
    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Set(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Record(String, Vec<Type>),
    Applied(Box<Type>, Vec<Type>),
    StructuralRecord(Box<Type>),
    RowEmpty,
    RowExtend(String, Box<Type>, Box<Type>),
    Generic(String),
    Named(String),
    Var(u32),
    RowVar(u32),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KnownType {
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Bool,
    String,
    Unit,
    Dynamic,
    Null,
    List(Box<KnownType>),
    Map(Box<KnownType>, Box<KnownType>),
    Set(Box<KnownType>),
    Function,
    Record(String, Vec<KnownType>),
    StructuralRecord(Vec<(String, KnownType)>),
    Named(String),
}

impl Type {
    fn is_dynamic_like(&self) -> bool {
        matches!(self, Self::Dynamic | Self::Null | Self::Generic(_))
    }

    fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Byte | Self::Short | Self::Int | Self::Long | Self::Float | Self::Double
        )
    }

    fn is_integral(&self) -> bool {
        matches!(self, Self::Byte | Self::Short | Self::Int | Self::Long)
    }
}

#[derive(Clone, Debug)]
struct Binding {
    mutable: bool,
    ty: Type,
    generalized_vars: Vec<GenericVar>,
    constraints: Vec<Constraint>,
}

#[derive(Clone, Debug)]
struct RecordSchema {
    type_params: Vec<String>,
    fields: Vec<(String, Type)>,
}

#[derive(Clone, Debug)]
struct TypeClassMethodInfo {
    name: String,
    ty: Type,
}

#[derive(Clone, Debug)]
struct TypeClassInfo {
    type_params: Vec<String>,
    methods: Vec<TypeClassMethodInfo>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Constraint {
    class_name: String,
    arguments: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct InstanceInfo {
    provided: Constraint,
    requirements: Vec<Constraint>,
}

#[derive(Clone, Debug)]
struct ProofSignature {
    params: Vec<String>,
    proposition: Expr,
}

#[derive(Default)]
struct TypeChecker {
    scopes: Vec<HashMap<String, Binding>>,
    proof_scopes: Vec<HashMap<String, ProofSignature>>,
    record_schemas: HashMap<String, RecordSchema>,
    typeclasses: HashMap<String, TypeClassInfo>,
    instances: Vec<InstanceInfo>,
    current_module: Option<String>,
    next_var: u32,
    substitutions: HashMap<GenericVar, Type>,
}

#[derive(Clone, Debug)]
struct StoredType {
    ty: Type,
    generalized_vars: Vec<GenericVar>,
    constraints: Vec<Constraint>,
}

type ModuleTypeExports = HashMap<String, StoredType>;

thread_local! {
    static USER_MODULE_TYPES: RefCell<HashMap<String, ModuleTypeExports>> = RefCell::new(HashMap::new());
    static USER_RECORD_SCHEMAS: RefCell<HashMap<String, RecordSchema>> = RefCell::new(HashMap::new());
    static USER_BINDING_TYPES: RefCell<HashMap<String, StoredType>> = RefCell::new(HashMap::new());
    static USER_TYPECLASS_INFOS: RefCell<HashMap<String, TypeClassInfo>> = RefCell::new(HashMap::new());
    static USER_INSTANCE_INFOS: RefCell<Vec<InstanceInfo>> = const { RefCell::new(Vec::new()) };
    /// User-defined extension methods, keyed by
    /// `(normalised-receiver-type, method-name)`. The stored function
    /// type still carries `this` as its first parameter; the FieldAccess
    /// inference path drops that prefix before returning the type to
    /// the caller.
    static USER_EXTENSION_METHODS: RefCell<HashMap<(String, String), StoredType>> =
        RefCell::new(HashMap::new());
}

fn normalise_receiver_annotation(annotation: &TypeAnnotation) -> String {
    let trimmed = annotation.text.trim();
    let stripped = trimmed.strip_prefix('#').unwrap_or(trimmed);
    stripped
        .split(|ch: char| ch == '<' || ch.is_whitespace())
        .next()
        .unwrap_or("")
        .to_string()
}

fn dispatch_key_for_type(ty: &Type) -> Option<String> {
    Some(match ty {
        Type::Int | Type::Byte | Type::Short | Type::Long => "Int".to_string(),
        Type::Float | Type::Double => "Double".to_string(),
        Type::Bool => "Boolean".to_string(),
        Type::String => "String".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Null => "Null".to_string(),
        Type::List(_) => "List".to_string(),
        Type::Map(_, _) => "Map".to_string(),
        Type::Set(_) => "Set".to_string(),
        Type::Record(name, _) | Type::Named(name) => name.clone(),
        Type::Applied(inner, _) => dispatch_key_for_type(inner)?,
        _ => return None,
    })
}

fn register_user_extension_method(receiver_key: String, method_name: String, stored: StoredType) {
    USER_EXTENSION_METHODS.with(|registry| {
        registry
            .borrow_mut()
            .insert((receiver_key, method_name), stored);
    });
}

fn resolve_user_extension_method_type(receiver_key: &str, method: &str) -> Option<StoredType> {
    USER_EXTENSION_METHODS.with(|registry| {
        registry
            .borrow()
            .get(&(receiver_key.to_string(), method.to_string()))
            .cloned()
    })
}

pub fn clear_user_extension_methods() {
    USER_EXTENSION_METHODS.with(|registry| registry.borrow_mut().clear());
}

pub fn typecheck_program(expr: &Expr) -> Result<(), Diagnostic> {
    typecheck_program_with_bindings(expr, std::iter::empty::<(String, KnownType)>())
}

pub fn typecheck_program_with_bindings<I, S>(expr: &Expr, bindings: I) -> Result<(), Diagnostic>
where
    I: IntoIterator<Item = (S, KnownType)>,
    S: Into<String>,
{
    let mut checker = TypeChecker::default();
    checker.push_scope();
    checker.install_builtins();
    checker.record_schemas.extend(resolve_user_record_schemas());
    checker.typeclasses.extend(resolve_user_typeclass_infos());
    checker.instances.extend(resolve_user_instance_types());
    for (name, stored) in resolve_user_binding_types() {
        checker.declare(
            name,
            false,
            stored.ty,
            stored.generalized_vars,
            stored.constraints,
        );
    }
    for (binding, ty) in bindings {
        let binding = binding.into();
        if checker.lookup(&binding).is_none() {
            checker.declare(binding, false, from_known_type(ty), Vec::new(), Vec::new());
        }
    }
    let result = checker.infer_program(expr).map(|_| ());
    if result.is_ok() {
        export_user_record_schemas(checker.user_record_schemas());
        export_user_typeclass_infos(checker.user_typeclass_infos());
        export_user_instance_types(checker.user_instance_types());
        // Module-scoped translation units export their root bindings
        // under the module's name only — leaking them into
        // USER_BINDING_TYPES would let user code reference module
        // members without an explicit `import`, which contradicts
        // module semantics and previously masked builtin names like
        // `isEmpty` for List.
        if let Some(module) = &checker.current_module {
            export_current_module_types(module, checker.root_exports());
        } else {
            export_user_binding_types(checker.root_exports());
        }
    }
    result
}

pub fn typecheck_program_with_dynamic_bindings<I, S>(
    expr: &Expr,
    bindings: I,
) -> Result<(), Diagnostic>
where
    I: IntoIterator<Item = S>,
    S: Into<String>,
{
    typecheck_program_with_bindings(
        expr,
        bindings.into_iter().map(|name| (name, KnownType::Dynamic)),
    )
}

pub fn clear_user_module_types() {
    USER_MODULE_TYPES.with(|modules| modules.borrow_mut().clear());
}

pub fn clear_user_record_schemas() {
    USER_RECORD_SCHEMAS.with(|schemas| schemas.borrow_mut().clear());
}

pub fn clear_user_binding_types() {
    USER_BINDING_TYPES.with(|bindings| bindings.borrow_mut().clear());
}

pub fn clear_user_typeclass_infos() {
    USER_TYPECLASS_INFOS.with(|infos| infos.borrow_mut().clear());
    USER_INSTANCE_INFOS.with(|instances| instances.borrow_mut().clear());
}

impl TypeChecker {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.proof_scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.proof_scopes.pop();
    }

    fn declare(
        &mut self,
        name: String,
        mutable: bool,
        ty: Type,
        generalized_vars: Vec<GenericVar>,
        constraints: Vec<Constraint>,
    ) {
        self.scopes
            .last_mut()
            .expect("type checker always has a scope")
            .insert(
                name,
                Binding {
                    mutable,
                    ty,
                    generalized_vars,
                    constraints,
                },
            );
    }

    fn declare_mono(&mut self, name: String, mutable: bool, ty: Type) {
        self.declare(name, mutable, ty, Vec::new(), Vec::new());
    }

    fn declare_poly(&mut self, name: String, mutable: bool, ty: Type) {
        let generalized_vars = free_vars(&ty);
        self.declare(name, mutable, ty, generalized_vars, Vec::new());
    }

    fn declare_stored(&mut self, name: String, mutable: bool, stored: &StoredType) {
        self.declare(
            name,
            mutable,
            stored.ty.clone(),
            stored.generalized_vars.clone(),
            stored.constraints.clone(),
        );
    }

    fn lookup(&self, name: &str) -> Option<&Binding> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    fn lookup_proof_signature(&self, name: &str) -> Option<&ProofSignature> {
        self.proof_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
    }

    fn root_exports(&self) -> ModuleTypeExports {
        self.scopes
            .first()
            .into_iter()
            .flat_map(|scope| scope.iter())
            .map(|(name, binding)| {
                (
                    name.clone(),
                    StoredType {
                        ty: binding.ty.clone(),
                        generalized_vars: binding.generalized_vars.clone(),
                        constraints: binding.constraints.clone(),
                    },
                )
            })
            .collect()
    }

    fn user_record_schemas(&self) -> HashMap<String, RecordSchema> {
        self.record_schemas
            .iter()
            .filter(|(name, _)| name.as_str() != "Point")
            .map(|(name, schema)| (name.clone(), schema.clone()))
            .collect()
    }

    fn user_typeclass_infos(&self) -> HashMap<String, TypeClassInfo> {
        self.typeclasses.clone()
    }

    fn user_instance_types(&self) -> Vec<InstanceInfo> {
        self.instances.clone()
    }

    fn fresh_var(&mut self) -> Type {
        let id = self.next_var;
        self.next_var += 1;
        Type::Var(id)
    }

    fn fresh_row_var(&mut self) -> Type {
        let id = self.next_var;
        self.next_var += 1;
        Type::RowVar(id)
    }

    /// Walk a `Pattern` against a scrutinee type, declaring any
    /// variable bindings in the current scope and unifying literal
    /// patterns with the scrutinee type. Constructor sub-patterns
    /// are bound with `Type::Dynamic` until we track variant arg
    /// types in a real enum schema (deferred — same fidelity as the
    /// pre-Pattern code which forced all bindings to `Dynamic`).
    fn declare_pattern_bindings(
        &mut self,
        pattern: &klassic_syntax::Pattern,
        scrutinee_type: &Type,
    ) -> Result<(), Diagnostic> {
        match pattern {
            klassic_syntax::Pattern::Wildcard { .. } => Ok(()),
            klassic_syntax::Pattern::Variable { name, .. } => {
                self.declare(
                    name.clone(),
                    false,
                    scrutinee_type.clone(),
                    Vec::new(),
                    Vec::new(),
                );
                Ok(())
            }
            klassic_syntax::Pattern::Constructor { args, .. } => {
                for arg in args {
                    self.declare_pattern_bindings(arg, &Type::Dynamic)?;
                }
                Ok(())
            }
            klassic_syntax::Pattern::LiteralInt { span, .. } => {
                let _ = self.unify(scrutinee_type.clone(), Type::Int, *span)?;
                Ok(())
            }
            klassic_syntax::Pattern::LiteralString { span, .. } => {
                let _ = self.unify(scrutinee_type.clone(), Type::String, *span)?;
                Ok(())
            }
            klassic_syntax::Pattern::LiteralBool { span, .. } => {
                let _ = self.unify(scrutinee_type.clone(), Type::Bool, *span)?;
                Ok(())
            }
        }
    }

    fn resolve(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => match self.substitutions.get(&GenericVar::Type(*id)) {
                Some(next) => self.resolve(next),
                None => Type::Var(*id),
            },
            Type::RowVar(id) => match self.substitutions.get(&GenericVar::Row(*id)) {
                Some(next) => self.resolve(next),
                None => Type::RowVar(*id),
            },
            Type::List(inner) => Type::List(Box::new(self.resolve(inner))),
            Type::Map(key, value) => {
                Type::Map(Box::new(self.resolve(key)), Box::new(self.resolve(value)))
            }
            Type::Set(inner) => Type::Set(Box::new(self.resolve(inner))),
            Type::Function(params, result) => Type::Function(
                params.iter().map(|param| self.resolve(param)).collect(),
                Box::new(self.resolve(result)),
            ),
            Type::Applied(head, args) => Type::Applied(
                Box::new(self.resolve(head)),
                args.iter().map(|arg| self.resolve(arg)).collect(),
            ),
            Type::Record(name, args) => Type::Record(
                name.clone(),
                args.iter().map(|arg| self.resolve(arg)).collect(),
            ),
            Type::StructuralRecord(row) => Type::StructuralRecord(Box::new(self.resolve(row))),
            Type::RowExtend(label, field, rest) => Type::RowExtend(
                label.clone(),
                Box::new(self.resolve(field)),
                Box::new(self.resolve(rest)),
            ),
            other => other.clone(),
        }
    }

    fn bind_var(&mut self, var: GenericVar, ty: Type) {
        self.substitutions.insert(var, ty);
    }

    fn default_unresolved_type(&mut self, ty: Type, default: Type) -> Type {
        match self.resolve(&ty) {
            Type::Var(id) => {
                self.bind_var(GenericVar::Type(id), default.clone());
                default
            }
            other => other,
        }
    }

    fn instantiate(&mut self, binding: &Binding) -> Type {
        if binding.generalized_vars.is_empty() {
            return self.resolve(&binding.ty);
        }
        let mut replacements = HashMap::new();
        for var in &binding.generalized_vars {
            let replacement = match var {
                GenericVar::Type(_) => self.fresh_var(),
                GenericVar::Row(_) => self.fresh_row_var(),
            };
            replacements.insert(var.clone(), replacement);
        }
        replace_generic_vars(&self.resolve(&binding.ty), &replacements)
    }

    fn instantiate_binding_signature(&mut self, binding: &Binding) -> (Type, Vec<Constraint>) {
        if binding.generalized_vars.is_empty() {
            return (
                self.resolve(&binding.ty),
                binding
                    .constraints
                    .iter()
                    .map(|constraint| self.resolve_constraint(constraint))
                    .collect(),
            );
        }
        let mut replacements = HashMap::new();
        for var in &binding.generalized_vars {
            let replacement = match var {
                GenericVar::Type(_) => self.fresh_var(),
                GenericVar::Row(_) => self.fresh_row_var(),
            };
            replacements.insert(var.clone(), replacement);
        }
        (
            replace_generic_vars(&self.resolve(&binding.ty), &replacements),
            binding
                .constraints
                .iter()
                .map(|constraint| replace_constraint_generics(constraint, &replacements))
                .collect(),
        )
    }

    fn instantiate_stored_type(&mut self, stored: &StoredType) -> Type {
        if stored.generalized_vars.is_empty() {
            return self.resolve(&stored.ty);
        }
        let mut replacements = HashMap::new();
        for var in &stored.generalized_vars {
            let replacement = match var {
                GenericVar::Type(_) => self.fresh_var(),
                GenericVar::Row(_) => self.fresh_row_var(),
            };
            replacements.insert(var.clone(), replacement);
        }
        replace_generic_vars(&self.resolve(&stored.ty), &replacements)
    }

    fn free_vars_in_env(&self) -> Vec<GenericVar> {
        let mut vars = Vec::new();
        for scope in &self.scopes {
            for binding in scope.values() {
                for var in free_vars(&binding.ty) {
                    if !binding.generalized_vars.contains(&var) && !vars.contains(&var) {
                        vars.push(var);
                    }
                }
                for var in free_vars_in_constraints(&binding.constraints) {
                    if !binding.generalized_vars.contains(&var) && !vars.contains(&var) {
                        vars.push(var);
                    }
                }
            }
        }
        vars
    }

    fn generalize_signature(&self, ty: &Type, constraints: &[Constraint]) -> Vec<GenericVar> {
        let env_vars = self.free_vars_in_env();
        let mut signature_vars = free_vars(ty);
        for var in free_vars_in_constraints(constraints) {
            if !signature_vars.contains(&var) {
                signature_vars.push(var);
            }
        }
        signature_vars
            .into_iter()
            .filter(|var| !env_vars.contains(var))
            .collect()
    }

    fn resolve_constraint(&self, constraint: &Constraint) -> Constraint {
        Constraint {
            class_name: constraint.class_name.clone(),
            arguments: constraint
                .arguments
                .iter()
                .map(|argument| self.resolve(argument))
                .collect(),
        }
    }

    fn unify(&mut self, lhs: Type, rhs: Type, span: Span) -> Result<Type, Diagnostic> {
        let lhs = self.resolve(&lhs);
        let rhs = self.resolve(&rhs);
        if lhs == rhs || lhs.is_dynamic_like() || rhs.is_dynamic_like() {
            return Ok(if lhs.is_dynamic_like() { rhs } else { lhs });
        }
        match (lhs, rhs) {
            (left, right) if left.is_numeric() && right.is_numeric() => {
                Ok(promote_numeric(left, right))
            }
            (Type::Var(id), ty) | (ty, Type::Var(id)) => {
                if occurs_in(GenericVar::Type(id), &ty) {
                    return Err(type_error(span, "recursive type"));
                }
                if is_row_type(&ty) {
                    return Err(type_error(span, "cannot bind type variable to row type"));
                }
                self.bind_var(GenericVar::Type(id), ty.clone());
                Ok(ty)
            }
            (Type::RowVar(id), ty) | (ty, Type::RowVar(id)) => {
                if occurs_in(GenericVar::Row(id), &ty) {
                    return Err(type_error(span, "recursive row type"));
                }
                if !is_row_type(&ty) {
                    return Err(type_error(span, "row variable expects a row type"));
                }
                self.bind_var(GenericVar::Row(id), ty.clone());
                Ok(ty)
            }
            (Type::List(a), Type::List(b)) => {
                let inner = self.unify(*a, *b, span)?;
                Ok(Type::List(Box::new(inner)))
            }
            (Type::Map(ak, av), Type::Map(bk, bv)) => {
                let key = self.unify(*ak, *bk, span)?;
                let value = self.unify(*av, *bv, span)?;
                Ok(Type::Map(Box::new(key), Box::new(value)))
            }
            (Type::Set(a), Type::Set(b)) => {
                let inner = self.unify(*a, *b, span)?;
                Ok(Type::Set(Box::new(inner)))
            }
            (Type::Bool, Type::Prop) | (Type::Prop, Type::Bool) | (Type::Prop, Type::Prop) => {
                Ok(Type::Prop)
            }
            (Type::Applied(left_head, left_args), Type::List(inner)) if left_args.len() == 1 => {
                let head = self.unify(*left_head, Type::Named("List".to_string()), span)?;
                let arg = self.unify(left_args.into_iter().next().unwrap(), *inner, span)?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::List(inner), Type::Applied(right_head, right_args)) if right_args.len() == 1 => {
                let head = self.unify(Type::Named("List".to_string()), *right_head, span)?;
                let arg = self.unify(*inner, right_args.into_iter().next().unwrap(), span)?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::Applied(left_head, left_args), Type::Set(inner)) if left_args.len() == 1 => {
                let head = self.unify(*left_head, Type::Named("Set".to_string()), span)?;
                let arg = self.unify(left_args.into_iter().next().unwrap(), *inner, span)?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::Set(inner), Type::Applied(right_head, right_args)) if right_args.len() == 1 => {
                let head = self.unify(Type::Named("Set".to_string()), *right_head, span)?;
                let arg = self.unify(*inner, right_args.into_iter().next().unwrap(), span)?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::Applied(left_head, left_args), Type::Map(key, value))
                if left_args.len() == 2 =>
            {
                let head = self.unify(*left_head, Type::Named("Map".to_string()), span)?;
                let mut args = left_args.into_iter();
                let key_ty = self.unify(args.next().unwrap(), *key, span)?;
                let value_ty = self.unify(args.next().unwrap(), *value, span)?;
                Ok(Type::Applied(Box::new(head), vec![key_ty, value_ty]))
            }
            (Type::Map(key, value), Type::Applied(right_head, right_args))
                if right_args.len() == 2 =>
            {
                let head = self.unify(Type::Named("Map".to_string()), *right_head, span)?;
                let mut args = right_args.into_iter();
                let key_ty = self.unify(*key, args.next().unwrap(), span)?;
                let value_ty = self.unify(*value, args.next().unwrap(), span)?;
                Ok(Type::Applied(Box::new(head), vec![key_ty, value_ty]))
            }
            (Type::Function(mut ap, ar), Type::Function(mut bp, br)) if ap.len() == bp.len() => {
                let mut params = Vec::with_capacity(ap.len());
                for (left, right) in ap.drain(..).zip(bp.drain(..)) {
                    params.push(self.unify(left, right, span)?);
                }
                let result = self.unify(*ar, *br, span)?;
                Ok(Type::Function(params, Box::new(result)))
            }
            (Type::Applied(left_head, left_args), Type::Applied(right_head, right_args))
                if left_args.len() == right_args.len() =>
            {
                let head = self.unify(*left_head, *right_head, span)?;
                let mut args = Vec::with_capacity(left_args.len());
                for (left, right) in left_args.into_iter().zip(right_args) {
                    args.push(self.unify(left, right, span)?);
                }
                Ok(Type::Applied(Box::new(head), args))
            }
            (Type::Record(left_name, left_args), Type::Record(right_name, right_args))
                if left_name == right_name =>
            {
                self.unify_record_types(left_name, left_args, right_args, span)
            }
            (Type::StructuralRecord(left), Type::Record(name, args)) => {
                let row = self
                    .structural_row_for_record_type(&name, &args)
                    .ok_or_else(|| type_error(span, format!("unknown record `{name}`")))?;
                self.unify(
                    Type::StructuralRecord(left),
                    Type::StructuralRecord(Box::new(row)),
                    span,
                )
            }
            (Type::Record(name, args), Type::StructuralRecord(right)) => {
                let row = self
                    .structural_row_for_record_type(&name, &args)
                    .ok_or_else(|| type_error(span, format!("unknown record `{name}`")))?;
                self.unify(
                    Type::StructuralRecord(Box::new(row)),
                    Type::StructuralRecord(right),
                    span,
                )
            }
            (Type::StructuralRecord(left), Type::StructuralRecord(right)) => {
                let row = self.unify(*left, *right, span)?;
                Ok(Type::StructuralRecord(Box::new(row)))
            }
            (Type::RowEmpty, Type::RowEmpty) => Ok(Type::RowEmpty),
            (Type::RowExtend(label, field, rest), row) if is_row_type(&row) => {
                let (other_field, other_rest) = self.rewrite_row(row, &label, span)?;
                let unified_field = self.unify(*field, other_field, span)?;
                let unified_rest = self.unify(*rest, other_rest, span)?;
                Ok(Type::RowExtend(
                    label,
                    Box::new(unified_field),
                    Box::new(unified_rest),
                ))
            }
            (row, Type::RowExtend(label, field, rest)) if is_row_type(&row) => {
                let (other_field, other_rest) = self.rewrite_row(row, &label, span)?;
                let unified_field = self.unify(other_field, *field, span)?;
                let unified_rest = self.unify(other_rest, *rest, span)?;
                Ok(Type::RowExtend(
                    label,
                    Box::new(unified_field),
                    Box::new(unified_rest),
                ))
            }
            (left, right) => Err(type_error(
                span,
                format!(
                    "type mismatch: {} is not compatible with {}",
                    display_type(&left),
                    display_type(&right)
                ),
            )),
        }
    }

    fn enforce_assignable(
        &mut self,
        expected: Type,
        actual: Type,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        let expected = self.resolve(&expected);
        let actual = self.resolve(&actual);
        if expected == actual || expected.is_dynamic_like() || actual.is_dynamic_like() {
            return Ok(if expected.is_dynamic_like() {
                actual
            } else {
                expected
            });
        }
        match (expected, actual) {
            (Type::Var(id), ty) => {
                if occurs_in(GenericVar::Type(id), &ty) {
                    return Err(type_error(span, "recursive type"));
                }
                if is_row_type(&ty) {
                    return Err(type_error(span, "cannot bind type variable to row type"));
                }
                self.bind_var(GenericVar::Type(id), ty.clone());
                Ok(ty)
            }
            (ty, Type::Var(id)) => {
                if occurs_in(GenericVar::Type(id), &ty) {
                    return Err(type_error(span, "recursive type"));
                }
                if is_row_type(&ty) {
                    return Err(type_error(span, "cannot bind type variable to row type"));
                }
                self.bind_var(GenericVar::Type(id), ty.clone());
                Ok(ty)
            }
            (Type::RowVar(id), ty) => {
                if occurs_in(GenericVar::Row(id), &ty) {
                    return Err(type_error(span, "recursive row type"));
                }
                if !is_row_type(&ty) {
                    return Err(type_error(span, "row variable expects a row type"));
                }
                self.bind_var(GenericVar::Row(id), ty.clone());
                Ok(ty)
            }
            (ty, Type::RowVar(id)) => {
                if occurs_in(GenericVar::Row(id), &ty) {
                    return Err(type_error(span, "recursive row type"));
                }
                if !is_row_type(&ty) {
                    return Err(type_error(span, "row variable expects a row type"));
                }
                self.bind_var(GenericVar::Row(id), ty.clone());
                Ok(ty)
            }
            (Type::List(a), Type::List(b)) => {
                let inner = self.enforce_assignable(*a, *b, span)?;
                Ok(Type::List(Box::new(inner)))
            }
            (Type::Map(ak, av), Type::Map(bk, bv)) => {
                let key = self.enforce_assignable(*ak, *bk, span)?;
                let value = self.enforce_assignable(*av, *bv, span)?;
                Ok(Type::Map(Box::new(key), Box::new(value)))
            }
            (Type::Set(a), Type::Set(b)) => {
                let inner = self.enforce_assignable(*a, *b, span)?;
                Ok(Type::Set(Box::new(inner)))
            }
            (Type::Bool, Type::Prop) | (Type::Prop, Type::Bool) | (Type::Prop, Type::Prop) => {
                Ok(Type::Prop)
            }
            (Type::Applied(expected_head, expected_args), Type::List(inner))
                if expected_args.len() == 1 =>
            {
                let head =
                    self.enforce_assignable(*expected_head, Type::Named("List".to_string()), span)?;
                let arg = self.enforce_assignable(
                    expected_args.into_iter().next().unwrap(),
                    *inner,
                    span,
                )?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::List(inner), Type::Applied(actual_head, actual_args))
                if actual_args.len() == 1 =>
            {
                let head =
                    self.enforce_assignable(Type::Named("List".to_string()), *actual_head, span)?;
                let arg =
                    self.enforce_assignable(*inner, actual_args.into_iter().next().unwrap(), span)?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::Applied(expected_head, expected_args), Type::Set(inner))
                if expected_args.len() == 1 =>
            {
                let head =
                    self.enforce_assignable(*expected_head, Type::Named("Set".to_string()), span)?;
                let arg = self.enforce_assignable(
                    expected_args.into_iter().next().unwrap(),
                    *inner,
                    span,
                )?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::Set(inner), Type::Applied(actual_head, actual_args))
                if actual_args.len() == 1 =>
            {
                let head =
                    self.enforce_assignable(Type::Named("Set".to_string()), *actual_head, span)?;
                let arg =
                    self.enforce_assignable(*inner, actual_args.into_iter().next().unwrap(), span)?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::Applied(expected_head, expected_args), Type::Map(key, value))
                if expected_args.len() == 2 =>
            {
                let head =
                    self.enforce_assignable(*expected_head, Type::Named("Map".to_string()), span)?;
                let mut args = expected_args.into_iter();
                let key_ty = self.enforce_assignable(args.next().unwrap(), *key, span)?;
                let value_ty = self.enforce_assignable(args.next().unwrap(), *value, span)?;
                Ok(Type::Applied(Box::new(head), vec![key_ty, value_ty]))
            }
            (Type::Map(key, value), Type::Applied(actual_head, actual_args))
                if actual_args.len() == 2 =>
            {
                let head =
                    self.enforce_assignable(Type::Named("Map".to_string()), *actual_head, span)?;
                let mut args = actual_args.into_iter();
                let key_ty = self.enforce_assignable(*key, args.next().unwrap(), span)?;
                let value_ty = self.enforce_assignable(*value, args.next().unwrap(), span)?;
                Ok(Type::Applied(Box::new(head), vec![key_ty, value_ty]))
            }
            (Type::Function(mut ap, ar), Type::Function(mut bp, br)) if ap.len() == bp.len() => {
                let mut params = Vec::with_capacity(ap.len());
                for (left, right) in ap.drain(..).zip(bp.drain(..)) {
                    params.push(self.enforce_assignable(left, right, span)?);
                }
                let result = self.enforce_assignable(*ar, *br, span)?;
                Ok(Type::Function(params, Box::new(result)))
            }
            (
                Type::Applied(expected_head, expected_args),
                Type::Applied(actual_head, actual_args),
            ) if expected_args.len() == actual_args.len() => {
                let head = self.enforce_assignable(*expected_head, *actual_head, span)?;
                let mut args = Vec::with_capacity(expected_args.len());
                for (expected, actual) in expected_args.into_iter().zip(actual_args) {
                    args.push(self.enforce_assignable(expected, actual, span)?);
                }
                Ok(Type::Applied(Box::new(head), args))
            }
            (Type::Record(left_name, left_args), Type::Record(right_name, right_args))
                if left_name == right_name =>
            {
                self.unify_record_types(left_name, left_args, right_args, span)
            }
            (Type::StructuralRecord(expected_row), Type::Record(name, args)) => {
                let actual_row = self
                    .structural_row_for_record_type(&name, &args)
                    .ok_or_else(|| type_error(span, format!("unknown record `{name}`")))?;
                self.enforce_assignable(
                    Type::StructuralRecord(expected_row),
                    Type::StructuralRecord(Box::new(actual_row)),
                    span,
                )
            }
            (Type::Record(name, args), Type::StructuralRecord(actual_row)) => {
                let expected_row = self
                    .structural_row_for_record_type(&name, &args)
                    .ok_or_else(|| type_error(span, format!("unknown record `{name}`")))?;
                self.enforce_assignable(
                    Type::StructuralRecord(Box::new(expected_row)),
                    Type::StructuralRecord(actual_row),
                    span,
                )
            }
            (Type::StructuralRecord(left), Type::StructuralRecord(right)) => {
                let row = self.enforce_assignable(*left, *right, span)?;
                Ok(Type::StructuralRecord(Box::new(row)))
            }
            (Type::RowEmpty, Type::RowEmpty) => Ok(Type::RowEmpty),
            (Type::RowExtend(label, field, rest), row) if is_row_type(&row) => {
                let (other_field, other_rest) = self.rewrite_row(row, &label, span)?;
                let unified_field = self.enforce_assignable(*field, other_field, span)?;
                let unified_rest = self.enforce_assignable(*rest, other_rest, span)?;
                Ok(Type::RowExtend(
                    label,
                    Box::new(unified_field),
                    Box::new(unified_rest),
                ))
            }
            (row, Type::RowExtend(label, field, rest)) if is_row_type(&row) => {
                let (other_field, other_rest) = self.rewrite_row(row, &label, span)?;
                let unified_field = self.enforce_assignable(other_field, *field, span)?;
                let unified_rest = self.enforce_assignable(other_rest, *rest, span)?;
                Ok(Type::RowExtend(
                    label,
                    Box::new(unified_field),
                    Box::new(unified_rest),
                ))
            }
            (left, right) => Err(type_error(
                span,
                format!(
                    "type mismatch: {} is not compatible with {}",
                    display_type(&left),
                    display_type(&right)
                ),
            )),
        }
    }

    fn unify_record_types(
        &mut self,
        name: String,
        left_args: Vec<Type>,
        right_args: Vec<Type>,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        if left_args.is_empty() {
            return Ok(Type::Record(name, right_args));
        }
        if right_args.is_empty() {
            return Ok(Type::Record(name, left_args));
        }
        if left_args.len() != right_args.len() {
            return Err(type_error(
                span,
                format!(
                    "type mismatch: #{} has {} parameters but #{} has {}",
                    name,
                    left_args.len(),
                    name,
                    right_args.len()
                ),
            ));
        }
        let mut args = Vec::with_capacity(left_args.len());
        for (left, right) in left_args.into_iter().zip(right_args) {
            args.push(self.enforce_assignable(left, right, span)?);
        }
        Ok(Type::Record(name, args))
    }

    fn rewrite_row(
        &mut self,
        row: Type,
        new_label: &str,
        span: Span,
    ) -> Result<(Type, Type), Diagnostic> {
        match self.resolve(&row) {
            Type::RowEmpty => Err(type_error(
                span,
                format!("field `{new_label}` is not available on this record"),
            )),
            Type::RowExtend(label, field, rest) if label == new_label => Ok((*field, *rest)),
            Type::RowExtend(label, field, rest) => {
                let (found_field, rewritten_rest) = self.rewrite_row(*rest, new_label, span)?;
                Ok((
                    found_field,
                    Type::RowExtend(label, field, Box::new(rewritten_rest)),
                ))
            }
            Type::RowVar(id) => {
                let tail = self.fresh_row_var();
                let field = self.fresh_var();
                let rewritten = Type::RowExtend(
                    new_label.to_string(),
                    Box::new(field.clone()),
                    Box::new(tail.clone()),
                );
                self.bind_var(GenericVar::Row(id), rewritten);
                Ok((field, tail))
            }
            other => Err(type_error(
                span,
                format!("{} is not a row type", display_type(&other)),
            )),
        }
    }

    fn infer_program(&mut self, expr: &Expr) -> Result<Type, Diagnostic> {
        match expr {
            Expr::Block { expressions, .. } => {
                self.predeclare_proofs(expressions);
                let mut last = Type::Unit;
                for expression in expressions {
                    last = self.infer_expr(expression)?;
                }
                Ok(last)
            }
            other => self.infer_expr(other),
        }
    }

    fn infer_expr(&mut self, expr: &Expr) -> Result<Type, Diagnostic> {
        match expr {
            Expr::Int { kind, .. } => Ok(match kind {
                IntLiteralKind::Byte => Type::Byte,
                IntLiteralKind::Short => Type::Short,
                IntLiteralKind::Int => Type::Int,
                IntLiteralKind::Long => Type::Long,
            }),
            Expr::Double { kind, .. } => Ok(match kind {
                FloatLiteralKind::Float => Type::Float,
                FloatLiteralKind::Double => Type::Double,
            }),
            Expr::Bool { .. } => Ok(Type::Bool),
            Expr::String { .. } => Ok(Type::String),
            Expr::Null { .. } => Ok(Type::Null),
            Expr::Unit { .. } => Ok(Type::Unit),
            Expr::Identifier { name, span } => {
                if let Some(binding) = self.lookup(name).cloned() {
                    Ok(self.instantiate(&binding))
                } else if let Some(stored) = resolve_module_selector_type(name) {
                    Ok(self.instantiate_stored_type(&stored))
                } else {
                    Err(type_error(*span, format!("undefined variable `{name}`")))
                }
            }
            Expr::ModuleHeader { name, .. } => {
                self.current_module = Some(name.clone());
                Ok(Type::Unit)
            }
            Expr::Import {
                path,
                alias,
                members,
                excludes,
                span,
            } => {
                self.import_module_types(
                    path,
                    alias.as_deref(),
                    members.as_deref(),
                    excludes,
                    *span,
                )?;
                Ok(Type::Unit)
            }
            Expr::TypeClassDeclaration {
                name,
                type_params,
                methods,
                ..
            } => {
                let info = self.build_typeclass_info(type_params, methods);
                self.typeclasses.insert(name.clone(), info);
                for method in methods {
                    self.declare_mono(method.name.clone(), false, Type::Dynamic);
                }
                Ok(Type::Unit)
            }
            Expr::InstanceDeclaration {
                class_name,
                for_type_annotation,
                constraints,
                span,
                ..
            } => {
                self.register_instance(class_name, for_type_annotation, constraints, *span)?;
                Ok(Type::Unit)
            }
            Expr::TheoremDeclaration {
                name,
                params,
                param_annotations,
                proposition,
                body,
                ..
            } => {
                let proof_type = self
                    .lookup(name)
                    .map(|binding| self.resolve(&binding.ty))
                    .unwrap_or_else(|| self.proof_binding_type(param_annotations));
                let param_types = match proof_type {
                    Type::Function(param_types, _) => param_types,
                    Type::Prop => Vec::new(),
                    _ => unreachable!("proof bindings are always proposition-shaped"),
                };
                self.push_scope();
                for (param, param_type) in params.iter().zip(param_types.iter()) {
                    self.declare_mono(param.clone(), false, param_type.clone());
                }
                let proposition_type = self.infer_expr(proposition)?;
                self.unify(Type::Bool, proposition_type, proposition.span())?;
                let body_type = self.infer_expr(body)?;
                match self.resolve(&body_type) {
                    Type::Unit => {}
                    other => {
                        self.unify(Type::Prop, other, body.span())?;
                        if !self.proof_body_matches_proposition(body, proposition) {
                            return Err(type_error(
                                body.span(),
                                "proof body does not establish declared proposition".to_string(),
                            ));
                        }
                    }
                }
                self.pop_scope();
                self.assign_declared_binding(
                    name,
                    if param_types.is_empty() {
                        Type::Prop
                    } else {
                        Type::Function(
                            param_types
                                .iter()
                                .map(|param| self.resolve(param))
                                .collect(),
                            Box::new(Type::Prop),
                        )
                    },
                );
                Ok(Type::Unit)
            }
            Expr::AxiomDeclaration {
                name,
                params,
                param_annotations,
                proposition,
                ..
            } => {
                let proof_type = self
                    .lookup(name)
                    .map(|binding| self.resolve(&binding.ty))
                    .unwrap_or_else(|| self.proof_binding_type(param_annotations));
                let param_types = match proof_type {
                    Type::Function(param_types, _) => param_types,
                    Type::Prop => Vec::new(),
                    _ => unreachable!("proof bindings are always proposition-shaped"),
                };
                self.push_scope();
                for (param, param_type) in params.iter().zip(param_types.iter()) {
                    self.declare_mono(param.clone(), false, param_type.clone());
                }
                let proposition_type = self.infer_expr(proposition)?;
                self.unify(Type::Bool, proposition_type, proposition.span())?;
                self.pop_scope();
                self.assign_declared_binding(
                    name,
                    if param_types.is_empty() {
                        Type::Prop
                    } else {
                        Type::Function(
                            param_types
                                .iter()
                                .map(|param| self.resolve(param))
                                .collect(),
                            Box::new(Type::Prop),
                        )
                    },
                );
                Ok(Type::Unit)
            }
            Expr::PegRuleBlock { .. } => Ok(Type::Unit),
            Expr::EnumDeclaration { .. } => {
                // Real enum typecheck would register constructor
                // signatures + a sum type, but the MVP only adds
                // the constructors to the runtime scope via eval.
                // Typecheck accepts the declaration as Unit so the
                // syntax flows through.
                self.register_enum_constructors_as_dynamic(expr);
                Ok(Type::Unit)
            }
            Expr::Match {
                scrutinee, arms, ..
            } => {
                let scrutinee_type = self.infer_expr(scrutinee)?;
                let mut arm_type = self.fresh_var();
                for arm in arms {
                    self.push_scope();
                    self.declare_pattern_bindings(&arm.pattern, &scrutinee_type)?;
                    if let Some(guard) = &arm.guard {
                        let guard_type = self.infer_expr(guard)?;
                        let _ = self.unify(guard_type, Type::Bool, guard.span())?;
                    }
                    let body_type = self.infer_expr(&arm.body)?;
                    self.pop_scope();
                    arm_type = self.unify(arm_type, body_type, arm.span)?;
                }
                Ok(self.resolve(&arm_type))
            }
            Expr::ExtensionDeclaration {
                type_params,
                this_name,
                receiver_type,
                methods,
                span,
            } => self.infer_extension_declaration(
                type_params,
                this_name,
                receiver_type,
                methods,
                *span,
            ),
            Expr::RecordDeclaration {
                name,
                type_params,
                fields,
                ..
            } => {
                let schema = self.record_schema_from_fields(type_params, fields);
                self.record_schemas.insert(name.clone(), schema);
                Ok(Type::Unit)
            }
            Expr::RecordLiteral { fields, .. } => {
                let mut row = Type::RowEmpty;
                for (name, value) in fields.iter().rev() {
                    let value_type = self.infer_expr(value)?;
                    row = Type::RowExtend(
                        name.clone(),
                        Box::new(self.resolve(&value_type)),
                        Box::new(row),
                    );
                }
                Ok(Type::StructuralRecord(Box::new(row)))
            }
            Expr::VarDecl {
                mutable,
                name,
                annotation,
                value,
                span,
            } => {
                let value_type = self.infer_expr(value)?;
                let final_type = if let Some(annotation) = annotation {
                    let mut named = HashMap::new();
                    let declared = self.parse_annotation_with_named_vars(annotation, &mut named);
                    self.enforce_assignable(declared, value_type, *span)?
                } else {
                    value_type
                };
                let final_type = self.resolve(&final_type);
                let generalized_vars = if *mutable {
                    Vec::new()
                } else {
                    self.generalize_signature(&final_type, &[])
                };
                self.declare(
                    name.clone(),
                    *mutable,
                    final_type,
                    generalized_vars,
                    Vec::new(),
                );
                Ok(Type::Unit)
            }
            Expr::DefDecl {
                name,
                type_params: _,
                constraints,
                params,
                param_annotations,
                return_annotation,
                body,
                span,
            } => {
                let mut named = HashMap::new();
                let mut param_types = param_annotations
                    .iter()
                    .map(|annotation| {
                        annotation
                            .as_ref()
                            .map(|annotation| {
                                self.parse_annotation_with_named_vars(annotation, &mut named)
                            })
                            .unwrap_or_else(|| self.fresh_var())
                    })
                    .collect::<Vec<_>>();
                if param_types.len() < params.len() {
                    param_types.resize_with(params.len(), || self.fresh_var());
                }
                let return_type = return_annotation
                    .as_ref()
                    .map(|annotation| self.parse_annotation_with_named_vars(annotation, &mut named))
                    .unwrap_or_else(|| self.fresh_var());
                let resolved_constraints =
                    self.build_constraints(constraints, &mut named, *span)?;
                let function_type =
                    Type::Function(param_types.clone(), Box::new(return_type.clone()));
                self.push_scope();
                self.declare(
                    name.clone(),
                    false,
                    function_type.clone(),
                    Vec::new(),
                    resolved_constraints.clone(),
                );
                for (param, ty) in params.iter().zip(param_types.iter()) {
                    self.declare(param.clone(), false, ty.clone(), Vec::new(), Vec::new());
                }
                self.declare_constraint_methods(&resolved_constraints, *span)?;
                let body_type = self.infer_expr(body)?;
                self.pop_scope();

                let resolved_return = self.enforce_assignable(return_type, body_type, *span)?;
                let resolved_params = param_types
                    .into_iter()
                    .map(|param| self.resolve(&param))
                    .collect();
                let final_type =
                    Type::Function(resolved_params, Box::new(self.resolve(&resolved_return)));
                let generalized_vars =
                    self.generalize_signature(&final_type, &resolved_constraints);
                self.declare(
                    name.clone(),
                    false,
                    final_type,
                    generalized_vars,
                    resolved_constraints,
                );
                Ok(Type::Unit)
            }
            Expr::Lambda {
                params,
                param_annotations,
                body,
                span: _,
            } => {
                let mut named = HashMap::new();
                let mut param_types = param_annotations
                    .iter()
                    .map(|annotation| {
                        annotation
                            .as_ref()
                            .map(|annotation| {
                                self.parse_annotation_with_named_vars(annotation, &mut named)
                            })
                            .unwrap_or_else(|| self.fresh_var())
                    })
                    .collect::<Vec<_>>();
                if param_types.len() < params.len() {
                    param_types.resize_with(params.len(), || self.fresh_var());
                }
                self.push_scope();
                for (param, ty) in params.iter().zip(param_types.iter()) {
                    self.declare(param.clone(), false, ty.clone(), Vec::new(), Vec::new());
                }
                let body_type = self.infer_expr(body)?;
                self.pop_scope();
                let resolved_params = param_types
                    .into_iter()
                    .map(|param| self.resolve(&param))
                    .collect();
                Ok(Type::Function(
                    resolved_params,
                    Box::new(self.resolve(&body_type)),
                ))
            }
            Expr::Assign {
                name, value, span, ..
            } => {
                let Some(binding) = self.lookup(name).cloned() else {
                    return Err(type_error(*span, format!("undefined variable `{name}`")));
                };
                if !binding.mutable {
                    return Err(type_error(
                        *span,
                        format!("cannot assign to immutable binding `{name}`"),
                    ));
                }
                let value_type = self.infer_expr(value)?;
                self.enforce_assignable(binding.ty, value_type, *span)
            }
            Expr::Unary { op, expr, span } => {
                let inner = self.infer_expr(expr)?;
                match op {
                    klassic_syntax::UnaryOp::Not => {
                        self.unify(Type::Bool, inner, *span)?;
                        Ok(Type::Bool)
                    }
                    klassic_syntax::UnaryOp::Plus | klassic_syntax::UnaryOp::Minus => {
                        let resolved = self.default_unresolved_type(inner, Type::Int);
                        if resolved.is_numeric() || resolved.is_dynamic_like() {
                            Ok(resolved)
                        } else {
                            Err(type_error(*span, "unary numeric operator expects a number"))
                        }
                    }
                }
            }
            Expr::Binary { lhs, op, rhs, span } => {
                let left = self.infer_expr(lhs)?;
                let right = self.infer_expr(rhs)?;
                use klassic_syntax::BinaryOp;
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if matches!(op, BinaryOp::Add)
                            && (matches!(self.resolve(&left), Type::String)
                                || matches!(self.resolve(&right), Type::String))
                        {
                            return Ok(Type::String);
                        }
                        let resolved_left = self.resolve(&left);
                        let resolved_right = self.resolve(&right);
                        if resolved_left.is_integral()
                            && resolved_right.is_integral()
                            && resolved_left != resolved_right
                        {
                            return Err(type_error(
                                *span,
                                format!(
                                    "type mismatch: {} is not compatible with {}",
                                    display_type(&resolved_left),
                                    display_type(&resolved_right)
                                ),
                            ));
                        }
                        let unified = self.unify(left.clone(), right.clone(), *span)?;
                        let resolved = self.default_unresolved_type(unified, Type::Int);
                        if resolved.is_numeric() || resolved.is_dynamic_like() {
                            Ok(resolved)
                        } else {
                            Err(type_error(*span, "arithmetic operator expects numbers"))
                        }
                    }
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                        let unified = self.unify(left, right, *span)?;
                        let resolved = self.default_unresolved_type(unified, Type::Int);
                        if resolved.is_integral() || resolved.is_dynamic_like() {
                            Ok(resolved)
                        } else {
                            Err(type_error(*span, "bitwise operator expects integers"))
                        }
                    }
                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        self.unify(Type::Bool, left, *span)?;
                        self.unify(Type::Bool, right, *span)?;
                        Ok(Type::Bool)
                    }
                    BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual
                    | BinaryOp::Equal
                    | BinaryOp::NotEqual => {
                        let _ = self.unify(left, right, *span)?;
                        Ok(Type::Bool)
                    }
                }
            }
            Expr::Call {
                callee,
                arguments,
                span,
            } => {
                if let Expr::Identifier { name, .. } = callee.as_ref()
                    && let Some(binding) = self.lookup(name).cloned()
                    && !binding.constraints.is_empty()
                {
                    let (callee_type, constraints) = self.instantiate_binding_signature(&binding);
                    let result = self.infer_constrained_call(callee_type, arguments, *span)?;
                    self.ensure_constraints_satisfied(&constraints, *span)?;
                    return Ok(result);
                }
                let callee_type = self.infer_expr(callee)?;
                self.infer_call(callee_type, arguments, *span)
            }
            Expr::FieldAccess {
                target,
                field,
                span,
            } => {
                let target_type = self.infer_expr(target)?;
                if let Some(method_type) = self.builtin_value_method_type(&target_type, field) {
                    return Ok(method_type);
                }
                if let Some(method_type) = self.user_extension_method_type(&target_type, field) {
                    return Ok(method_type);
                }
                match self.resolve(&target_type) {
                    Type::Record(name, args) => self
                        .lookup_record_field_type(&name, &args, field)
                        .ok_or_else(|| {
                            type_error(
                                *span,
                                format!("record `{name}` does not have a `{field}` field"),
                            )
                        }),
                    Type::StructuralRecord(row) => {
                        let field_type = self.fresh_var();
                        let row_tail = self.fresh_row_var();
                        let expected = Type::StructuralRecord(Box::new(Type::RowExtend(
                            field.clone(),
                            Box::new(field_type.clone()),
                            Box::new(row_tail),
                        )));
                        let _ = self.unify(Type::StructuralRecord(row), expected, *span)?;
                        Ok(self.resolve(&field_type))
                    }
                    Type::Named(name) => self
                        .lookup_record_field_type(&name, &[], field)
                        .ok_or_else(|| {
                            type_error(
                                *span,
                                format!("record `{name}` does not have a `{field}` field"),
                            )
                        }),
                    Type::Dynamic | Type::Null | Type::Var(_) | Type::RowVar(_) => {
                        let field_type = self.fresh_var();
                        let row_tail = self.fresh_row_var();
                        let expected = Type::StructuralRecord(Box::new(Type::RowExtend(
                            field.clone(),
                            Box::new(field_type.clone()),
                            Box::new(row_tail),
                        )));
                        let _ = self.unify(target_type, expected, *span)?;
                        Ok(self.resolve(&field_type))
                    }
                    other => Err(type_error(
                        *span,
                        format!("{} does not support field access", display_type(&other)),
                    )),
                }
            }
            Expr::Cleanup {
                body,
                cleanup,
                span: _,
            } => {
                let body_type = self.infer_expr(body)?;
                let _ = self.infer_expr(cleanup)?;
                Ok(self.resolve(&body_type))
            }
            Expr::RecordConstructor {
                name,
                arguments,
                span,
            } => self.infer_record_constructor(name, arguments, *span),
            Expr::ListLiteral { elements, .. } => {
                let mut element_type = self.fresh_var();
                for element in elements {
                    let ty = self.infer_expr(element)?;
                    element_type = self.unify(element_type, ty, element.span())?;
                }
                Ok(Type::List(Box::new(self.resolve(&element_type))))
            }
            Expr::MapLiteral { entries, .. } => {
                let mut key_type = self.fresh_var();
                let mut value_type = self.fresh_var();
                for (key, value) in entries {
                    let inferred_key = self.infer_expr(key)?;
                    key_type = self.unify(key_type, inferred_key, key.span())?;
                    let inferred_value = self.infer_expr(value)?;
                    value_type = self.unify(value_type, inferred_value, value.span())?;
                }
                Ok(Type::Map(
                    Box::new(self.resolve(&key_type)),
                    Box::new(self.resolve(&value_type)),
                ))
            }
            Expr::SetLiteral { elements, .. } => {
                let mut element_type = self.fresh_var();
                for element in elements {
                    let inferred = self.infer_expr(element)?;
                    element_type = self.unify(element_type, inferred, element.span())?;
                }
                Ok(Type::Set(Box::new(self.resolve(&element_type))))
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let condition_type = self.infer_expr(condition)?;
                self.unify(Type::Bool, condition_type, condition.span())?;
                let then_type = self.infer_expr(then_branch)?;
                if let Some(branch) = else_branch {
                    let else_type = self.infer_expr(branch)?;
                    self.unify(then_type, else_type, *span)
                } else {
                    Ok(Type::Unit)
                }
            }
            Expr::While {
                condition, body, ..
            } => {
                let condition_type = self.infer_expr(condition)?;
                self.unify(Type::Bool, condition_type, condition.span())?;
                let _ = self.infer_expr(body)?;
                Ok(Type::Unit)
            }
            Expr::Foreach {
                binding,
                iterable,
                body,
                ..
            } => {
                let iterable_type = self.infer_expr(iterable)?;
                let element_type = match self.resolve(&iterable_type) {
                    Type::List(inner) => *inner,
                    _ => Type::Dynamic,
                };
                self.push_scope();
                self.declare_mono(binding.clone(), false, element_type);
                let _ = self.infer_expr(body)?;
                self.pop_scope();
                Ok(Type::Unit)
            }
            Expr::Block { expressions, .. } => {
                self.push_scope();
                self.predeclare_proofs(expressions);
                let mut last = Type::Unit;
                for expression in expressions {
                    last = self.infer_expr(expression)?;
                }
                self.pop_scope();
                Ok(last)
            }
        }
    }

    fn infer_call(
        &mut self,
        callee_type: Type,
        arguments: &[Expr],
        span: Span,
    ) -> Result<Type, Diagnostic> {
        match self.resolve(&callee_type) {
            Type::Function(params, result) => {
                if arguments.len() != params.len() {
                    return Err(type_error(
                        span,
                        format!(
                            "function expects {} arguments but got {}",
                            params.len(),
                            arguments.len()
                        ),
                    ));
                }
                for (expected, argument) in params.into_iter().zip(arguments.iter()) {
                    self.check_call_argument(expected, argument)?;
                }
                Ok(self.resolve(&result))
            }
            Type::Dynamic | Type::Null | Type::Var(_) => Ok(Type::Dynamic),
            other => Err(type_error(
                span,
                format!("{} is not callable", display_type(&other)),
            )),
        }
    }

    fn predeclare_proofs(&mut self, expressions: &[Expr]) {
        for expression in expressions {
            match expression {
                Expr::TheoremDeclaration {
                    name,
                    params,
                    param_annotations,
                    proposition,
                    ..
                }
                | Expr::AxiomDeclaration {
                    name,
                    params,
                    param_annotations,
                    proposition,
                    ..
                } => {
                    self.proof_scopes
                        .last_mut()
                        .expect("type checker always has a proof scope")
                        .insert(
                            name.clone(),
                            ProofSignature {
                                params: params.clone(),
                                proposition: (**proposition).clone(),
                            },
                        );
                    if self.lookup(name).is_none() {
                        let proof_type = self.proof_binding_type(param_annotations);
                        self.declare_mono(name.clone(), false, proof_type);
                    }
                }
                _ => {}
            }
        }
    }

    fn proof_binding_type(&mut self, param_annotations: &[Option<TypeAnnotation>]) -> Type {
        let mut named = HashMap::new();
        let params = param_annotations
            .iter()
            .map(|annotation| match annotation {
                Some(annotation) => self.parse_annotation_with_named_vars(annotation, &mut named),
                None => self.fresh_var(),
            })
            .collect::<Vec<_>>();
        if params.is_empty() {
            Type::Prop
        } else {
            Type::Function(params, Box::new(Type::Prop))
        }
    }

    fn assign_declared_binding(&mut self, name: &str, ty: Type) {
        let resolved = self.resolve(&ty);
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.ty = resolved;
                binding.generalized_vars.clear();
                binding.constraints.clear();
                return;
            }
        }
        self.declare_mono(name.to_string(), false, resolved);
    }

    fn proof_body_matches_proposition(&self, body: &Expr, proposition: &Expr) -> bool {
        expr_equivalent_ignoring_spans(body, proposition)
            || self
                .infer_proven_proposition(body)
                .is_some_and(|proved| expr_equivalent_ignoring_spans(&proved, proposition))
    }

    fn infer_proven_proposition(&self, expr: &Expr) -> Option<Expr> {
        match expr {
            Expr::Identifier { name, .. } => {
                let signature = self.lookup_proof_signature(name)?;
                if signature.params.is_empty() {
                    Some(signature.proposition.clone())
                } else {
                    None
                }
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return None;
                };
                let signature = self.lookup_proof_signature(name)?;
                if signature.params.len() != arguments.len() {
                    return None;
                }
                let substitutions = signature
                    .params
                    .iter()
                    .cloned()
                    .zip(arguments.iter().cloned())
                    .collect::<HashMap<_, _>>();
                Some(substitute_expr_identifiers(
                    &signature.proposition,
                    &substitutions,
                ))
            }
            _ => None,
        }
    }

    fn infer_constrained_call(
        &mut self,
        callee_type: Type,
        arguments: &[Expr],
        span: Span,
    ) -> Result<Type, Diagnostic> {
        match self.resolve(&callee_type) {
            Type::Function(params, result) => {
                if arguments.len() != params.len() {
                    return Err(type_error(
                        span,
                        format!(
                            "function expects {} arguments but got {}",
                            params.len(),
                            arguments.len()
                        ),
                    ));
                }
                for (expected, argument) in params.into_iter().zip(arguments.iter()) {
                    self.check_call_argument(expected, argument)?;
                }
                Ok(self.resolve(&result))
            }
            Type::Dynamic | Type::Null | Type::Var(_) => Ok(Type::Dynamic),
            other => Err(type_error(
                span,
                format!("{} is not callable", display_type(&other)),
            )),
        }
    }

    fn check_call_argument(&mut self, expected: Type, argument: &Expr) -> Result<(), Diagnostic> {
        if let Type::Function(expected_params, expected_result) = self.resolve(&expected)
            && let Expr::Lambda {
                params,
                param_annotations,
                body,
                ..
            } = argument
            && params.len() == expected_params.len()
        {
            let mut named = HashMap::new();
            let mut param_types = Vec::with_capacity(params.len());
            for ((param, expected), annotation) in params
                .iter()
                .zip(expected_params)
                .zip(param_annotations.iter())
            {
                let param_type = if let Some(annotation) = annotation {
                    let declared = self.parse_annotation_with_named_vars(annotation, &mut named);
                    self.enforce_assignable(expected, declared.clone(), argument.span())?
                } else {
                    expected
                };
                param_types.push((param.clone(), self.resolve(&param_type)));
            }

            self.push_scope();
            for (param, param_type) in param_types {
                self.declare(param, false, param_type, Vec::new(), Vec::new());
            }
            let result = match self.infer_expr(body) {
                Ok(body_type) => self
                    .enforce_assignable(*expected_result, body_type, argument.span())
                    .map(|_| ()),
                Err(error) => Err(error),
            };
            self.pop_scope();
            return result;
        }

        let actual = self.infer_expr(argument)?;
        self.match_call_argument_type(expected, actual, argument.span())
    }

    fn match_call_argument_type(
        &mut self,
        expected: Type,
        actual: Type,
        span: Span,
    ) -> Result<(), Diagnostic> {
        match self.resolve(&expected) {
            Type::Var(id) => {
                let actual = self.resolve(&actual);
                if matches!(actual, Type::Var(other_id) if other_id == id)
                    || occurs_in(GenericVar::Type(id), &actual)
                {
                    return Ok(());
                }
                if is_row_type(&actual) {
                    return Err(type_error(span, "cannot bind type variable to row type"));
                }
                self.bind_var(GenericVar::Type(id), actual);
                Ok(())
            }
            Type::RowVar(id) => {
                let actual = self.resolve(&actual);
                if matches!(actual, Type::RowVar(other_id) if other_id == id) {
                    return Ok(());
                }
                if occurs_in(GenericVar::Row(id), &actual) {
                    return Err(type_error(span, "recursive row type"));
                }
                if !is_row_type(&actual) {
                    return Err(type_error(span, "row variable expects a row type"));
                }
                self.bind_var(GenericVar::Row(id), actual);
                Ok(())
            }
            expected => self.enforce_assignable(expected, actual, span).map(|_| ()),
        }
    }

    fn install_builtins(&mut self) {
        self.record_schemas.insert(
            "Point".to_string(),
            RecordSchema {
                type_params: Vec::new(),
                fields: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
            },
        );
        self.declare_poly(
            "println".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "printlnError".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "ToDo".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Dynamic)),
        );
        self.declare_poly(
            "assert".to_string(),
            false,
            Type::Function(vec![Type::Bool], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "assertResult".to_string(),
            false,
            Type::Function(
                vec![Type::Dynamic],
                Box::new(Type::Function(vec![Type::Dynamic], Box::new(Type::Unit))),
            ),
        );
        self.declare_poly(
            "thread".to_string(),
            false,
            Type::Function(
                vec![Type::Function(vec![], Box::new(Type::Dynamic))],
                Box::new(Type::Unit),
            ),
        );
        self.declare_poly(
            "sleep".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "__gc_alloc".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_record".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_array".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_concat".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_println".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "__gc_list_int".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_len".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_set".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "__gc_list_int_get".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_println".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "__gc_list_int_push".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_pop".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_reverse".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_concat".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_len".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_alloc".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_get_byte".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_set_byte".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "__gc_string_eq".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "__gc_string_substring".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_repeat".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_index_of".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_index_of_from".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_last_index_of".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_to_int".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_int_to_string".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_starts_with".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "__gc_string_ends_with".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "__gc_string_contains".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "__gc_pointer_count".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_segment_count".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_collect_count".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr_len".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr_set".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "__gc_list_ptr_get".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr_get_string".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr_push".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr_pop".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr_concat".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr_reverse".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_ptr_join".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_sum".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_min".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_max".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_split".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_lines".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_replace".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_trim".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_to_lower".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_string_to_upper".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_list_int_to_string".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_smap_new".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_smap_size".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_smap_has".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "__gc_smap_get".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_smap_get_string".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_smap_set".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_smap_keys".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_smap_values".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_collect".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "__gc_pin".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_unpin".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "__gc_read".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_read_ptr".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_read_string".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__gc_write".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "stopwatch".to_string(),
            false,
            Type::Function(
                vec![Type::Function(vec![], Box::new(Type::Dynamic))],
                Box::new(Type::Int),
            ),
        );
        self.declare_poly(
            "Time#nowMillis".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "Math#powInt".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "Math#sqrtInt".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "Math#gcd".to_string(),
            false,
            Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "String#parseInt".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Int)),
        );
        self.declare_poly(
            "Random#seed".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Unit)),
        );
        self.declare_poly(
            "Random#nextInt".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "double".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Double)),
        );
        self.declare_poly(
            "int".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
        );
        self.declare_poly(
            "floor".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
        );
        self.declare_poly(
            "ceil".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
        );
        self.declare_poly(
            "sqrt".to_string(),
            false,
            Type::Function(vec![Type::Double], Box::new(Type::Double)),
        );
        self.declare_poly(
            "abs".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
        );
        self.declare_poly(
            "toString".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::String)),
        );
        self.declare_poly(
            "substring".to_string(),
            false,
            Type::Function(
                vec![Type::String, Type::Int, Type::Int],
                Box::new(Type::String),
            ),
        );
        self.declare_poly(
            "at".to_string(),
            false,
            Type::Function(vec![Type::String, Type::Int], Box::new(Type::String)),
        );
        self.declare_poly(
            "matches".to_string(),
            false,
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "split".to_string(),
            false,
            Type::Function(
                vec![Type::String, Type::String],
                Box::new(Type::List(Box::new(Type::String))),
            ),
        );
        self.declare_poly(
            "join".to_string(),
            false,
            Type::Function(
                vec![Type::List(Box::new(Type::String)), Type::String],
                Box::new(Type::String),
            ),
        );
        self.declare_poly(
            "trim".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        self.declare_poly(
            "trimLeft".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        self.declare_poly(
            "trimRight".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        self.declare_poly(
            "replace".to_string(),
            false,
            Type::Function(
                vec![Type::String, Type::String, Type::String],
                Box::new(Type::String),
            ),
        );
        self.declare_poly(
            "replaceAll".to_string(),
            false,
            Type::Function(
                vec![Type::String, Type::String, Type::String],
                Box::new(Type::String),
            ),
        );
        self.declare_poly(
            "toLowerCase".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        self.declare_poly(
            "toUpperCase".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        self.declare_poly(
            "startsWith".to_string(),
            false,
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "endsWith".to_string(),
            false,
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "contains".to_string(),
            false,
            Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "isEmptyString".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "indexOf".to_string(),
            false,
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Int)),
        );
        self.declare_poly(
            "lastIndexOf".to_string(),
            false,
            Type::Function(vec![Type::String, Type::String], Box::new(Type::Int)),
        );
        self.declare_poly(
            "length".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Int)),
        );
        self.declare_poly(
            "repeat".to_string(),
            false,
            Type::Function(vec![Type::String, Type::Int], Box::new(Type::String)),
        );
        self.declare_poly(
            "reverse".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        let a = self.fresh_var();
        let b = self.fresh_var();
        self.declare_poly(
            "head".to_string(),
            false,
            Type::Function(vec![Type::List(Box::new(a.clone()))], Box::new(a.clone())),
        );
        self.declare_poly(
            "tail".to_string(),
            false,
            Type::Function(
                vec![Type::List(Box::new(a.clone()))],
                Box::new(Type::List(Box::new(a.clone()))),
            ),
        );
        self.declare_poly(
            "size".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
        );
        self.declare_poly(
            "isEmpty".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "cons".to_string(),
            false,
            Type::Function(
                vec![a.clone()],
                Box::new(Type::Function(
                    vec![Type::List(Box::new(a.clone()))],
                    Box::new(Type::List(Box::new(a.clone()))),
                )),
            ),
        );
        self.declare_poly(
            "map".to_string(),
            false,
            Type::Function(
                vec![Type::Dynamic],
                Box::new(Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic))),
            ),
        );
        self.declare_poly(
            "foldLeft".to_string(),
            false,
            Type::Function(
                vec![Type::List(Box::new(a.clone()))],
                Box::new(Type::Function(
                    vec![b.clone()],
                    Box::new(Type::Function(
                        vec![Type::Function(vec![b.clone(), a], Box::new(b.clone()))],
                        Box::new(b),
                    )),
                )),
            ),
        );
        self.install_module_imports("Map", None, None, &[]);
        self.install_module_imports("Set", None, None, &[]);
        self.install_module_imports("FileInput", None, None, &[]);
        self.install_module_imports("FileOutput", None, None, &[]);
        self.declare_poly(
            "stdin".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::String)),
        );
        self.declare_poly(
            "stdinLines".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
        );
        self.install_module_imports("StandardInput", Some("StandardInput"), None, &[]);
        self.declare_poly(
            "env".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
        );
        self.declare_poly(
            "getEnv".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::String)),
        );
        self.declare_poly(
            "hasEnv".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Bool)),
        );
        self.install_module_imports("Environment", Some("Environment"), None, &[]);
        self.install_module_imports("CommandLine", None, None, &[]);
        self.install_module_imports("Process", None, None, &[]);
        self.install_module_imports("Dir", None, None, &[]);
    }

    fn install_module_imports(
        &mut self,
        path: &str,
        alias: Option<&str>,
        members: Option<&[String]>,
        excludes: &[String],
    ) {
        let entries: &[(&str, Type)] = match path {
            "Map" => &[
                (
                    "containsKey",
                    Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Bool)),
                ),
                (
                    "containsValue",
                    Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Bool)),
                ),
                (
                    "get",
                    Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Dynamic)),
                ),
                (
                    "isEmpty",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Bool)),
                ),
                (
                    "size",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
                ),
            ],
            "Set" => &[
                (
                    "contains",
                    Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Bool)),
                ),
                (
                    "isEmpty",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Bool)),
                ),
                (
                    "size",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
                ),
            ],
            "FileInput" => &[
                (
                    "open",
                    Type::Function(
                        vec![
                            Type::String,
                            Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
                        ],
                        Box::new(Type::Dynamic),
                    ),
                ),
                (
                    "readAll",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::String)),
                ),
                (
                    "readLines",
                    Type::Function(
                        vec![Type::Dynamic],
                        Box::new(Type::List(Box::new(Type::String))),
                    ),
                ),
                (
                    "all",
                    Type::Function(vec![Type::String], Box::new(Type::String)),
                ),
                (
                    "lines",
                    Type::Function(
                        vec![Type::String],
                        Box::new(Type::List(Box::new(Type::String))),
                    ),
                ),
            ],
            "FileOutput" => &[
                (
                    "write",
                    Type::Function(vec![Type::String, Type::Dynamic], Box::new(Type::Unit)),
                ),
                (
                    "append",
                    Type::Function(vec![Type::String, Type::Dynamic], Box::new(Type::Unit)),
                ),
                (
                    "exists",
                    Type::Function(vec![Type::String], Box::new(Type::Bool)),
                ),
                (
                    "delete",
                    Type::Function(vec![Type::String], Box::new(Type::Unit)),
                ),
                (
                    "writeLines",
                    Type::Function(
                        vec![Type::String, Type::List(Box::new(Type::String))],
                        Box::new(Type::Unit),
                    ),
                ),
            ],
            "StandardInput" => &[
                ("all", Type::Function(vec![], Box::new(Type::String))),
                (
                    "lines",
                    Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
                ),
            ],
            "Environment" => &[
                (
                    "vars",
                    Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
                ),
                (
                    "get",
                    Type::Function(vec![Type::String], Box::new(Type::String)),
                ),
                (
                    "exists",
                    Type::Function(vec![Type::String], Box::new(Type::Bool)),
                ),
            ],
            "CommandLine" => &[(
                "args",
                Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
            )],
            "Process" => &[(
                "exit",
                Type::Function(vec![Type::Int], Box::new(Type::Unit)),
            )],
            "Dir" => &[
                ("current", Type::Function(vec![], Box::new(Type::String))),
                ("home", Type::Function(vec![], Box::new(Type::String))),
                ("temp", Type::Function(vec![], Box::new(Type::String))),
                (
                    "exists",
                    Type::Function(vec![Type::String], Box::new(Type::Bool)),
                ),
                (
                    "mkdir",
                    Type::Function(vec![Type::String], Box::new(Type::Unit)),
                ),
                (
                    "mkdirs",
                    Type::Function(vec![Type::String], Box::new(Type::Unit)),
                ),
                (
                    "isDirectory",
                    Type::Function(vec![Type::String], Box::new(Type::Bool)),
                ),
                (
                    "isFile",
                    Type::Function(vec![Type::String], Box::new(Type::Bool)),
                ),
                (
                    "list",
                    Type::Function(
                        vec![Type::String],
                        Box::new(Type::List(Box::new(Type::String))),
                    ),
                ),
                (
                    "listFull",
                    Type::Function(
                        vec![Type::String],
                        Box::new(Type::List(Box::new(Type::String))),
                    ),
                ),
                (
                    "delete",
                    Type::Function(vec![Type::String], Box::new(Type::Unit)),
                ),
                (
                    "copy",
                    Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)),
                ),
                (
                    "move",
                    Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)),
                ),
            ],
            _ => &[],
        };

        if let Some(alias) = alias {
            for (name, ty) in entries {
                let allowed = members
                    .is_none_or(|allowed| allowed.iter().any(|member| member == name))
                    && !excludes.iter().any(|member| member == name);
                if allowed {
                    self.declare_poly(format!("{alias}#{name}"), false, ty.clone());
                }
            }
        }
        for (name, ty) in entries {
            let allowed = members.is_none_or(|allowed| allowed.iter().any(|member| member == name))
                && !excludes.iter().any(|member| member == name);
            if alias.is_none() && allowed {
                self.declare_poly(
                    (*name).to_string(),
                    false,
                    imported_module_member_type(path, name, ty),
                );
                self.declare_poly(format!("{path}#{name}"), false, ty.clone());
            }
        }
    }

    fn import_module_types(
        &mut self,
        path: &str,
        alias: Option<&str>,
        members: Option<&[String]>,
        excludes: &[String],
        span: Span,
    ) -> Result<(), Diagnostic> {
        if builtin_module_type_exports(path).is_some() {
            self.install_module_imports(path, alias, members, excludes);
            return Ok(());
        }

        let exports = resolve_user_module_types(path).ok_or_else(|| {
            let message = if path.starts_with("std.") {
                format!(
                    "module `{path}` is part of the standard library but is not yet \
                     available in native builds. Run the program with the evaluator \
                     (klassic file.kl) or wait for the native stdlib module loader \
                     described in docs/roadmap-targets-stdlib.md."
                )
            } else {
                format!("unknown module `{path}`")
            };
            type_error(span, message)
        })?;

        if let Some(alias) = alias {
            for (name, stored) in &exports {
                let allowed = members
                    .is_none_or(|allowed| allowed.iter().any(|member| member == name))
                    && !excludes.iter().any(|member| member == name);
                if allowed {
                    self.declare_stored(format!("{alias}#{name}"), false, stored);
                }
            }
        }

        match members {
            Some(members) => {
                for member in members {
                    if excludes.iter().any(|excluded| excluded == member) {
                        continue;
                    }
                    let stored = exports.get(member).cloned().ok_or_else(|| {
                        type_error(span, format!("module `{path}` has no member `{member}`"))
                    })?;
                    self.declare(
                        member.clone(),
                        false,
                        stored.ty,
                        stored.generalized_vars,
                        stored.constraints,
                    );
                }
            }
            None if alias.is_none() => {
                for (name, stored) in exports {
                    if excludes.iter().any(|member| member == &name) {
                        continue;
                    }
                    self.declare(
                        name,
                        false,
                        stored.ty,
                        stored.generalized_vars,
                        stored.constraints,
                    );
                }
            }
            None => {}
        }

        Ok(())
    }

    fn build_typeclass_info(
        &self,
        type_params: &[String],
        methods: &[TypeClassMethod],
    ) -> TypeClassInfo {
        TypeClassInfo {
            type_params: type_params.to_vec(),
            methods: methods
                .iter()
                .map(|method| TypeClassMethodInfo {
                    name: method.name.clone(),
                    ty: parse_schema_type_annotation(&method.annotation.text, type_params),
                })
                .collect(),
        }
    }

    fn build_constraints(
        &mut self,
        constraints: &[TypeClassConstraint],
        named: &mut HashMap<String, Type>,
        span: Span,
    ) -> Result<Vec<Constraint>, Diagnostic> {
        let mut resolved = Vec::with_capacity(constraints.len());
        for constraint in constraints {
            let info = self
                .typeclasses
                .get(&constraint.class_name)
                .ok_or_else(|| {
                    type_error(
                        constraint.span,
                        format!("unknown typeclass `{}`", constraint.class_name),
                    )
                })?;
            if !info.type_params.is_empty() && info.type_params.len() != constraint.arguments.len()
            {
                return Err(type_error(
                    constraint.span,
                    format!(
                        "typeclass `{}` expects {} type arguments but got {}",
                        constraint.class_name,
                        info.type_params.len(),
                        constraint.arguments.len()
                    ),
                ));
            }
            let arguments = constraint
                .arguments
                .iter()
                .map(|argument| {
                    let parsed = self.parse_annotation_with_named_vars(argument, named);
                    self.normalize_constraint_type(parsed)
                })
                .collect();
            resolved.push(Constraint {
                class_name: constraint.class_name.clone(),
                arguments,
            });
        }
        let _ = span;
        Ok(resolved)
    }

    fn declare_constraint_methods(
        &mut self,
        constraints: &[Constraint],
        span: Span,
    ) -> Result<(), Diagnostic> {
        for constraint in constraints {
            let info = self
                .typeclasses
                .get(&constraint.class_name)
                .cloned()
                .ok_or_else(|| {
                    type_error(
                        span,
                        format!("unknown typeclass `{}`", constraint.class_name),
                    )
                })?;
            let substitutions: HashMap<String, Type> = info
                .type_params
                .iter()
                .cloned()
                .zip(constraint.arguments.iter().cloned())
                .collect();
            for method in &info.methods {
                self.declare_mono(
                    method.name.clone(),
                    false,
                    substitute_generics(&method.ty, &substitutions),
                );
            }
        }
        Ok(())
    }

    fn register_instance(
        &mut self,
        class_name: &str,
        for_type_annotation: &TypeAnnotation,
        constraints: &[TypeClassConstraint],
        span: Span,
    ) -> Result<(), Diagnostic> {
        let mut named = HashMap::new();
        let provided_type = self.parse_annotation_with_named_vars(for_type_annotation, &mut named);
        let provided = Constraint {
            class_name: class_name.to_string(),
            arguments: vec![self.normalize_constraint_type(provided_type)],
        };
        let requirements = self.build_constraints(constraints, &mut named, span)?;
        self.instances.push(InstanceInfo {
            provided,
            requirements,
        });
        Ok(())
    }

    fn normalize_constraint_type(&self, ty: Type) -> Type {
        match ty {
            Type::Named(name) if self.record_schemas.contains_key(&name) => {
                Type::Record(name, Vec::new())
            }
            Type::List(inner) => Type::List(Box::new(self.normalize_constraint_type(*inner))),
            Type::Set(inner) => Type::Set(Box::new(self.normalize_constraint_type(*inner))),
            Type::Map(key, value) => Type::Map(
                Box::new(self.normalize_constraint_type(*key)),
                Box::new(self.normalize_constraint_type(*value)),
            ),
            Type::Function(params, result) => Type::Function(
                params
                    .into_iter()
                    .map(|param| self.normalize_constraint_type(param))
                    .collect(),
                Box::new(self.normalize_constraint_type(*result)),
            ),
            Type::Record(name, args) => Type::Record(
                name,
                args.into_iter()
                    .map(|arg| self.normalize_constraint_type(arg))
                    .collect(),
            ),
            Type::StructuralRecord(row) => {
                Type::StructuralRecord(Box::new(self.normalize_constraint_type(*row)))
            }
            Type::RowExtend(label, field, rest) => Type::RowExtend(
                label,
                Box::new(self.normalize_constraint_type(*field)),
                Box::new(self.normalize_constraint_type(*rest)),
            ),
            other => other,
        }
    }

    fn ensure_constraints_satisfied(
        &self,
        constraints: &[Constraint],
        span: Span,
    ) -> Result<(), Diagnostic> {
        for constraint in constraints {
            let resolved = self.resolve_constraint(constraint);
            if !self.constraint_is_satisfied(&resolved, &mut Vec::new()) {
                let args = resolved
                    .arguments
                    .iter()
                    .map(display_type)
                    .collect::<Vec<_>>()
                    .join(", ");
                return Err(type_error(
                    span,
                    format!("missing instance for {}<{}>", resolved.class_name, args),
                ));
            }
        }
        Ok(())
    }

    fn constraint_is_satisfied(&self, required: &Constraint, stack: &mut [Constraint]) -> bool {
        if required
            .arguments
            .iter()
            .any(|argument| matches!(argument, Type::Dynamic | Type::Var(_) | Type::RowVar(_)))
        {
            return true;
        }
        if stack.contains(required) {
            return true;
        }
        for instance in self.instances.iter().rev() {
            let Some(replacements) = match_constraint_pattern(&instance.provided, required) else {
                continue;
            };
            let mut next_stack = stack.to_owned();
            next_stack.push(required.clone());
            let requirements = instance
                .requirements
                .iter()
                .map(|constraint| replace_constraint_generics(constraint, &replacements))
                .collect::<Vec<_>>();
            if requirements
                .iter()
                .all(|constraint| self.constraint_is_satisfied(constraint, &mut next_stack))
            {
                return true;
            }
        }
        false
    }

    fn parse_annotation_with_named_vars(
        &mut self,
        annotation: &TypeAnnotation,
        named: &mut HashMap<String, Type>,
    ) -> Type {
        parse_type_annotation_with_named_vars(&annotation.text, named, &mut self.next_var)
    }

    fn infer_record_constructor(
        &mut self,
        name: &str,
        arguments: &[Expr],
        span: Span,
    ) -> Result<Type, Diagnostic> {
        if let Some(schema) = self.record_schemas.get(name).cloned() {
            if arguments.len() != schema.fields.len() {
                return Err(type_error(
                    span,
                    format!(
                        "record `{name}` expects {} fields but got {}",
                        schema.fields.len(),
                        arguments.len()
                    ),
                ));
            }
            let mut substitutions = HashMap::new();
            for (argument, (_, expected)) in arguments.iter().zip(schema.fields.iter()) {
                let actual = self.infer_expr(argument)?;
                self.match_generic_annotation(
                    expected.clone(),
                    self.resolve(&actual),
                    argument.span(),
                    &mut substitutions,
                )?;
            }
            let type_args = schema
                .type_params
                .iter()
                .map(|param| substitutions.get(param).cloned().unwrap_or(Type::Dynamic))
                .collect();
            Ok(Type::Record(name.to_string(), type_args))
        } else {
            for argument in arguments {
                let _ = self.infer_expr(argument)?;
            }
            Ok(Type::Record(name.to_string(), Vec::new()))
        }
    }

    fn match_generic_annotation(
        &mut self,
        expected: Type,
        actual: Type,
        span: Span,
        substitutions: &mut HashMap<String, Type>,
    ) -> Result<Type, Diagnostic> {
        let expected = self.resolve(&expected);
        let actual = self.resolve(&actual);
        match (expected, actual) {
            (Type::Generic(name), actual) => {
                if let Some(bound) = substitutions.get(&name).cloned() {
                    self.enforce_assignable(bound, actual, span)
                } else {
                    substitutions.insert(name, actual.clone());
                    Ok(actual)
                }
            }
            (Type::Bool, Type::Prop) | (Type::Prop, Type::Bool) | (Type::Prop, Type::Prop) => {
                Ok(Type::Prop)
            }
            (Type::List(expected), Type::List(actual)) => {
                let inner =
                    self.match_generic_annotation(*expected, *actual, span, substitutions)?;
                Ok(Type::List(Box::new(inner)))
            }
            (Type::Map(expected_key, expected_value), Type::Map(actual_key, actual_value)) => {
                let key =
                    self.match_generic_annotation(*expected_key, *actual_key, span, substitutions)?;
                let value = self.match_generic_annotation(
                    *expected_value,
                    *actual_value,
                    span,
                    substitutions,
                )?;
                Ok(Type::Map(Box::new(key), Box::new(value)))
            }
            (Type::Set(expected), Type::Set(actual)) => {
                let inner =
                    self.match_generic_annotation(*expected, *actual, span, substitutions)?;
                Ok(Type::Set(Box::new(inner)))
            }
            (Type::Applied(expected_head, expected_args), Type::List(actual))
                if expected_args.len() == 1 =>
            {
                let head = self.match_generic_annotation(
                    *expected_head,
                    Type::Named("List".to_string()),
                    span,
                    substitutions,
                )?;
                let arg = self.match_generic_annotation(
                    expected_args.into_iter().next().unwrap(),
                    *actual,
                    span,
                    substitutions,
                )?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::Applied(expected_head, expected_args), Type::Set(actual))
                if expected_args.len() == 1 =>
            {
                let head = self.match_generic_annotation(
                    *expected_head,
                    Type::Named("Set".to_string()),
                    span,
                    substitutions,
                )?;
                let arg = self.match_generic_annotation(
                    expected_args.into_iter().next().unwrap(),
                    *actual,
                    span,
                    substitutions,
                )?;
                Ok(Type::Applied(Box::new(head), vec![arg]))
            }
            (Type::Applied(expected_head, expected_args), Type::Map(actual_key, actual_value))
                if expected_args.len() == 2 =>
            {
                let head = self.match_generic_annotation(
                    *expected_head,
                    Type::Named("Map".to_string()),
                    span,
                    substitutions,
                )?;
                let mut args = expected_args.into_iter();
                let key = self.match_generic_annotation(
                    args.next().unwrap(),
                    *actual_key,
                    span,
                    substitutions,
                )?;
                let value = self.match_generic_annotation(
                    args.next().unwrap(),
                    *actual_value,
                    span,
                    substitutions,
                )?;
                Ok(Type::Applied(Box::new(head), vec![key, value]))
            }
            (
                Type::Function(expected_params, expected_result),
                Type::Function(actual_params, actual_result),
            ) if expected_params.len() == actual_params.len() => {
                let mut params = Vec::with_capacity(expected_params.len());
                for (expected, actual) in expected_params.into_iter().zip(actual_params) {
                    params.push(self.match_generic_annotation(
                        expected,
                        actual,
                        span,
                        substitutions,
                    )?);
                }
                let result = self.match_generic_annotation(
                    *expected_result,
                    *actual_result,
                    span,
                    substitutions,
                )?;
                Ok(Type::Function(params, Box::new(result)))
            }
            (
                Type::Applied(expected_head, expected_args),
                Type::Applied(actual_head, actual_args),
            ) if expected_args.len() == actual_args.len() => {
                let head = self.match_generic_annotation(
                    *expected_head,
                    *actual_head,
                    span,
                    substitutions,
                )?;
                let mut args = Vec::with_capacity(expected_args.len());
                for (expected, actual) in expected_args.into_iter().zip(actual_args) {
                    args.push(self.match_generic_annotation(
                        expected,
                        actual,
                        span,
                        substitutions,
                    )?);
                }
                Ok(Type::Applied(Box::new(head), args))
            }
            (
                Type::Record(expected_name, expected_args),
                Type::Record(actual_name, actual_args),
            ) if expected_name == actual_name && expected_args.len() == actual_args.len() => {
                let mut args = Vec::with_capacity(expected_args.len());
                for (expected, actual) in expected_args.into_iter().zip(actual_args) {
                    args.push(self.match_generic_annotation(
                        expected,
                        actual,
                        span,
                        substitutions,
                    )?);
                }
                Ok(Type::Record(expected_name, args))
            }
            (expected, actual) => self.enforce_assignable(expected, actual, span),
        }
    }

    fn record_schema_from_fields(
        &mut self,
        type_params: &[String],
        fields: &[RecordField],
    ) -> RecordSchema {
        RecordSchema {
            type_params: type_params.to_vec(),
            fields: fields
                .iter()
                .map(|field| {
                    let ty = field
                        .annotation
                        .as_ref()
                        .map(|annotation| {
                            parse_schema_type_annotation(&annotation.text, type_params)
                        })
                        .unwrap_or(Type::Dynamic);
                    (field.name.clone(), ty)
                })
                .collect(),
        }
    }

    fn lookup_record_field_type(
        &self,
        record_name: &str,
        record_args: &[Type],
        field_name: &str,
    ) -> Option<Type> {
        let schema = self.record_schemas.get(record_name)?;
        let substitutions: HashMap<String, Type> = schema
            .type_params
            .iter()
            .cloned()
            .zip(record_args.iter().cloned())
            .collect();
        schema.fields.iter().find_map(|(name, ty)| {
            (name == field_name).then(|| {
                let ty = substitute_generics(ty, &substitutions);
                match ty {
                    Type::Function(params, result) if !params.is_empty() => {
                        Type::Function(params[1..].to_vec(), result)
                    }
                    other => other,
                }
            })
        })
    }

    fn structural_row_for_record_type(
        &self,
        record_name: &str,
        record_args: &[Type],
    ) -> Option<Type> {
        let schema = self.record_schemas.get(record_name)?;
        let substitutions: HashMap<String, Type> = schema
            .type_params
            .iter()
            .cloned()
            .zip(record_args.iter().cloned())
            .collect();
        let mut row = Type::RowEmpty;
        for (name, ty) in schema.fields.iter().rev() {
            row = Type::RowExtend(
                name.clone(),
                Box::new(substitute_generics(ty, &substitutions)),
                Box::new(row),
            );
        }
        Some(row)
    }

    fn register_enum_constructors_as_dynamic(&mut self, expr: &Expr) {
        let Expr::EnumDeclaration {
            name: _enum_name,
            variants,
            ..
        } = expr
        else {
            return;
        };
        for variant in variants {
            let arity = variant.params.len();
            let ty = if arity == 0 {
                Type::Dynamic
            } else {
                Type::Function(vec![Type::Dynamic; arity], Box::new(Type::Dynamic))
            };
            // Bind the variant name so user code can reference it
            // without typecheck errors. Once the proper sum-type
            // story lands this will register a real ADT constructor.
            self.declare_poly(variant.name.clone(), false, ty);
        }
    }

    fn infer_extension_declaration(
        &mut self,
        type_params: &[String],
        this_name: &str,
        receiver_type: &TypeAnnotation,
        methods: &[Expr],
        _span: Span,
    ) -> Result<Type, Diagnostic> {
        let receiver_key = normalise_receiver_annotation(receiver_type);
        for method in methods {
            let Expr::DefDecl {
                name,
                constraints,
                params,
                param_annotations,
                return_annotation,
                body,
                span,
                ..
            } = method
            else {
                continue;
            };

            let mut named = HashMap::new();
            for type_param in type_params {
                named
                    .entry(type_param.clone())
                    .or_insert_with(|| self.fresh_var());
            }
            let receiver_ty = self.parse_annotation_with_named_vars(receiver_type, &mut named);
            let mut param_types = vec![receiver_ty.clone()];
            for annotation in param_annotations {
                let ty = match annotation {
                    Some(annotation) => {
                        self.parse_annotation_with_named_vars(annotation, &mut named)
                    }
                    None => self.fresh_var(),
                };
                param_types.push(ty);
            }
            while param_types.len() < params.len() + 1 {
                param_types.push(self.fresh_var());
            }
            let return_type = return_annotation
                .as_ref()
                .map(|annotation| self.parse_annotation_with_named_vars(annotation, &mut named))
                .unwrap_or_else(|| self.fresh_var());
            let resolved_constraints = self.build_constraints(constraints, &mut named, *span)?;
            let function_type = Type::Function(param_types.clone(), Box::new(return_type.clone()));

            // Register the method's tentative type before checking the
            // body so recursive `this.method(...)` calls resolve.
            let preliminary_stored = StoredType {
                ty: function_type.clone(),
                generalized_vars: Vec::new(),
                constraints: resolved_constraints.clone(),
            };
            register_user_extension_method(receiver_key.clone(), name.clone(), preliminary_stored);

            self.push_scope();
            self.declare(
                name.clone(),
                false,
                function_type.clone(),
                Vec::new(),
                resolved_constraints.clone(),
            );
            self.declare(
                this_name.to_string(),
                false,
                receiver_ty.clone(),
                Vec::new(),
                Vec::new(),
            );
            for (param, ty) in params.iter().zip(param_types.iter().skip(1)) {
                self.declare(param.clone(), false, ty.clone(), Vec::new(), Vec::new());
            }
            self.declare_constraint_methods(&resolved_constraints, *span)?;
            let body_type = self.infer_expr(body)?;
            self.pop_scope();

            let resolved_return = self.enforce_assignable(return_type, body_type, *span)?;
            let resolved_params = param_types
                .into_iter()
                .map(|param| self.resolve(&param))
                .collect::<Vec<_>>();
            let final_type =
                Type::Function(resolved_params, Box::new(self.resolve(&resolved_return)));
            let generalized_vars = self.generalize_signature(&final_type, &resolved_constraints);
            let stored = StoredType {
                ty: final_type,
                generalized_vars,
                constraints: resolved_constraints,
            };
            register_user_extension_method(receiver_key.clone(), name.clone(), stored);
        }
        Ok(Type::Unit)
    }

    fn user_extension_method_type(&mut self, target: &Type, field: &str) -> Option<Type> {
        let resolved = self.resolve(target);
        let key = dispatch_key_for_type(&resolved)?;
        let stored = resolve_user_extension_method_type(&key, field)?;
        let instantiated = self.instantiate_stored_type(&stored);
        match instantiated {
            // Drop the leading `this` parameter — dispatch already
            // supplies the receiver, so the user-visible method type
            // only carries any extra arguments.
            Type::Function(mut params, ret) if !params.is_empty() => {
                params.remove(0);
                Some(Type::Function(params, ret))
            }
            other => Some(other),
        }
    }

    fn builtin_value_method_type(&mut self, target: &Type, field: &str) -> Option<Type> {
        let resolved = self.resolve(target);
        match resolved {
            Type::Int
            | Type::Double
            | Type::Bool
            | Type::Unit
            | Type::Record(_, _)
            | Type::Named(_) => match field {
                "toString" => Some(Type::Function(vec![], Box::new(Type::String))),
                _ => None,
            },
            Type::String => match field {
                "toString" => Some(Type::Function(vec![], Box::new(Type::String))),
                "substring" => Some(Type::Function(
                    vec![Type::Int, Type::Int],
                    Box::new(Type::String),
                )),
                "at" => Some(Type::Function(vec![Type::Int], Box::new(Type::String))),
                "matches" => Some(Type::Function(vec![Type::String], Box::new(Type::Bool))),
                "split" => Some(Type::Function(
                    vec![Type::String],
                    Box::new(Type::List(Box::new(Type::String))),
                )),
                "trim" | "trimLeft" | "trimRight" | "toLowerCase" | "toUpperCase" | "reverse" => {
                    Some(Type::Function(vec![], Box::new(Type::String)))
                }
                "replace" | "replaceAll" => Some(Type::Function(
                    vec![Type::String, Type::String],
                    Box::new(Type::String),
                )),
                "startsWith" | "endsWith" | "contains" => {
                    Some(Type::Function(vec![Type::String], Box::new(Type::Bool)))
                }
                "isEmpty" | "isEmptyString" => Some(Type::Function(vec![], Box::new(Type::Bool))),
                "indexOf" | "lastIndexOf" => {
                    Some(Type::Function(vec![Type::String], Box::new(Type::Int)))
                }
                "length" => Some(Type::Function(vec![], Box::new(Type::Int))),
                "repeat" => Some(Type::Function(vec![Type::Int], Box::new(Type::String))),
                _ => None,
            },
            Type::List(inner) => match field {
                "head" => Some(Type::Function(vec![], inner.clone())),
                "tail" => Some(Type::Function(vec![], Box::new(Type::List(inner.clone())))),
                "size" => Some(Type::Function(vec![], Box::new(Type::Int))),
                "isEmpty" => Some(Type::Function(vec![], Box::new(Type::Bool))),
                "contains" => Some(Type::Function(vec![Type::Dynamic], Box::new(Type::Bool))),
                "join" => Some(Type::Function(vec![Type::String], Box::new(Type::String))),
                "map" => Some(Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic))),
                "foldLeft" => Some(Type::Function(
                    vec![Type::Dynamic, Type::Dynamic],
                    Box::new(Type::Dynamic),
                )),
                _ => None,
            },
            Type::Map(_, _) => match field {
                "containsKey" | "containsValue" => {
                    Some(Type::Function(vec![Type::Dynamic], Box::new(Type::Bool)))
                }
                "get" => Some(Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic))),
                "isEmpty" => Some(Type::Function(vec![], Box::new(Type::Bool))),
                "size" => Some(Type::Function(vec![], Box::new(Type::Int))),
                _ => None,
            },
            Type::Set(_) => match field {
                "contains" => Some(Type::Function(vec![Type::Dynamic], Box::new(Type::Bool))),
                "isEmpty" => Some(Type::Function(vec![], Box::new(Type::Bool))),
                "size" => Some(Type::Function(vec![], Box::new(Type::Int))),
                _ => None,
            },
            Type::Var(id) => match field {
                "map" => {
                    let inner = self.fresh_var();
                    self.bind_var(GenericVar::Type(id), Type::List(Box::new(inner)));
                    Some(Type::Function(
                        vec![Type::Dynamic],
                        Box::new(Type::List(Box::new(Type::Dynamic))),
                    ))
                }
                "foldLeft" => {
                    let inner = self.fresh_var();
                    self.bind_var(GenericVar::Type(id), Type::List(Box::new(inner)));
                    Some(Type::Function(
                        vec![Type::Dynamic, Type::Dynamic],
                        Box::new(Type::Dynamic),
                    ))
                }
                "join" => {
                    self.bind_var(GenericVar::Type(id), Type::List(Box::new(Type::Dynamic)));
                    Some(Type::Function(vec![Type::String], Box::new(Type::String)))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

fn export_current_module_types(name: &str, exports: ModuleTypeExports) {
    USER_MODULE_TYPES.with(|modules| {
        modules.borrow_mut().insert(name.to_string(), exports);
    });
}

fn export_user_record_schemas(schemas: HashMap<String, RecordSchema>) {
    USER_RECORD_SCHEMAS.with(|known| {
        let mut known = known.borrow_mut();
        known.extend(schemas);
    });
}

fn export_user_binding_types(bindings: HashMap<String, StoredType>) {
    USER_BINDING_TYPES.with(|known| {
        let mut known = known.borrow_mut();
        known.extend(bindings);
    });
}

fn export_user_typeclass_infos(typeclasses: HashMap<String, TypeClassInfo>) {
    USER_TYPECLASS_INFOS.with(|known| {
        let mut known = known.borrow_mut();
        known.extend(typeclasses);
    });
}

fn export_user_instance_types(instances: Vec<InstanceInfo>) {
    USER_INSTANCE_INFOS.with(|known| {
        let mut known = known.borrow_mut();
        known.extend(instances);
    });
}

fn resolve_user_record_schemas() -> HashMap<String, RecordSchema> {
    USER_RECORD_SCHEMAS.with(|schemas| schemas.borrow().clone())
}

fn resolve_user_binding_types() -> HashMap<String, StoredType> {
    USER_BINDING_TYPES.with(|bindings| bindings.borrow().clone())
}

fn resolve_user_typeclass_infos() -> HashMap<String, TypeClassInfo> {
    USER_TYPECLASS_INFOS.with(|infos| infos.borrow().clone())
}

fn resolve_user_instance_types() -> Vec<InstanceInfo> {
    USER_INSTANCE_INFOS.with(|instances| instances.borrow().clone())
}

fn builtin_module_type_exports(path: &str) -> Option<ModuleTypeExports> {
    let entries: &[(&str, Type)] = match path {
        "Map" => &[
            (
                "containsKey",
                Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Bool)),
            ),
            (
                "containsValue",
                Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Bool)),
            ),
            (
                "get",
                Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Dynamic)),
            ),
            (
                "isEmpty",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Bool)),
            ),
            (
                "size",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
            ),
        ],
        "Set" => &[
            (
                "contains",
                Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Bool)),
            ),
            (
                "isEmpty",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Bool)),
            ),
            (
                "size",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
            ),
        ],
        "FileInput" => &[
            (
                "open",
                Type::Function(
                    vec![
                        Type::String,
                        Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
                    ],
                    Box::new(Type::Dynamic),
                ),
            ),
            (
                "readAll",
                Type::Function(vec![Type::Dynamic], Box::new(Type::String)),
            ),
            (
                "readLines",
                Type::Function(
                    vec![Type::Dynamic],
                    Box::new(Type::List(Box::new(Type::String))),
                ),
            ),
            (
                "all",
                Type::Function(vec![Type::String], Box::new(Type::String)),
            ),
            (
                "lines",
                Type::Function(
                    vec![Type::String],
                    Box::new(Type::List(Box::new(Type::String))),
                ),
            ),
        ],
        "FileOutput" => &[
            (
                "write",
                Type::Function(vec![Type::String, Type::Dynamic], Box::new(Type::Unit)),
            ),
            (
                "append",
                Type::Function(vec![Type::String, Type::Dynamic], Box::new(Type::Unit)),
            ),
            (
                "exists",
                Type::Function(vec![Type::String], Box::new(Type::Bool)),
            ),
            (
                "delete",
                Type::Function(vec![Type::String], Box::new(Type::Unit)),
            ),
            (
                "writeLines",
                Type::Function(
                    vec![Type::String, Type::List(Box::new(Type::String))],
                    Box::new(Type::Unit),
                ),
            ),
        ],
        "StandardInput" => &[
            ("all", Type::Function(vec![], Box::new(Type::String))),
            (
                "lines",
                Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
            ),
        ],
        "Environment" => &[
            (
                "vars",
                Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
            ),
            (
                "get",
                Type::Function(vec![Type::String], Box::new(Type::String)),
            ),
            (
                "exists",
                Type::Function(vec![Type::String], Box::new(Type::Bool)),
            ),
        ],
        "CommandLine" => &[(
            "args",
            Type::Function(vec![], Box::new(Type::List(Box::new(Type::String)))),
        )],
        "Process" => &[(
            "exit",
            Type::Function(vec![Type::Int], Box::new(Type::Unit)),
        )],
        "Dir" => &[
            ("current", Type::Function(vec![], Box::new(Type::String))),
            ("home", Type::Function(vec![], Box::new(Type::String))),
            ("temp", Type::Function(vec![], Box::new(Type::String))),
            (
                "exists",
                Type::Function(vec![Type::String], Box::new(Type::Bool)),
            ),
            (
                "mkdir",
                Type::Function(vec![Type::String], Box::new(Type::Unit)),
            ),
            (
                "mkdirs",
                Type::Function(vec![Type::String], Box::new(Type::Unit)),
            ),
            (
                "isDirectory",
                Type::Function(vec![Type::String], Box::new(Type::Bool)),
            ),
            (
                "isFile",
                Type::Function(vec![Type::String], Box::new(Type::Bool)),
            ),
            (
                "list",
                Type::Function(
                    vec![Type::String],
                    Box::new(Type::List(Box::new(Type::String))),
                ),
            ),
            (
                "listFull",
                Type::Function(
                    vec![Type::String],
                    Box::new(Type::List(Box::new(Type::String))),
                ),
            ),
            (
                "delete",
                Type::Function(vec![Type::String], Box::new(Type::Unit)),
            ),
            (
                "copy",
                Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)),
            ),
            (
                "move",
                Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)),
            ),
        ],
        _ => return None,
    };
    Some(
        entries
            .iter()
            .map(|(name, ty)| {
                (
                    (*name).to_string(),
                    StoredType {
                        ty: ty.clone(),
                        generalized_vars: free_vars(ty),
                        constraints: Vec::new(),
                    },
                )
            })
            .collect(),
    )
}

fn imported_module_member_type(path: &str, name: &str, ty: &Type) -> Type {
    match (path, name, ty) {
        ("Set", "contains", Type::Function(params, result)) if params.len() == 2 => Type::Function(
            vec![params[0].clone()],
            Box::new(Type::Function(vec![params[1].clone()], result.clone())),
        ),
        _ => ty.clone(),
    }
}

fn resolve_user_module_types(path: &str) -> Option<ModuleTypeExports> {
    USER_MODULE_TYPES.with(|modules| {
        let modules = modules.borrow();
        if let Some(module) = modules.get(path) {
            return Some(module.clone());
        }
        if path.contains('.') {
            return None;
        }
        let mut matches = modules
            .iter()
            .filter(|(name, _)| name.rsplit('.').next() == Some(path))
            .map(|(_, exports)| exports.clone());
        let first = matches.next()?;
        if matches.next().is_some() {
            None
        } else {
            Some(first)
        }
    })
}

fn resolve_module_selector_type(name: &str) -> Option<StoredType> {
    let (module, member) = name.split_once('#')?;
    builtin_module_type_exports(module)
        .or_else(|| resolve_user_module_types(module))
        .and_then(|exports| exports.get(member).cloned())
}

fn from_known_type(ty: KnownType) -> Type {
    match ty {
        KnownType::Byte => Type::Byte,
        KnownType::Short => Type::Short,
        KnownType::Int => Type::Int,
        KnownType::Long => Type::Long,
        KnownType::Float => Type::Float,
        KnownType::Double => Type::Double,
        KnownType::Bool => Type::Bool,
        KnownType::String => Type::String,
        KnownType::Unit => Type::Unit,
        KnownType::Dynamic => Type::Dynamic,
        KnownType::Null => Type::Null,
        KnownType::List(inner) => Type::List(Box::new(from_known_type(*inner))),
        KnownType::Map(key, value) => Type::Map(
            Box::new(from_known_type(*key)),
            Box::new(from_known_type(*value)),
        ),
        KnownType::Set(inner) => Type::Set(Box::new(from_known_type(*inner))),
        KnownType::Function => Type::Dynamic,
        KnownType::Record(name, args) => {
            Type::Record(name, args.into_iter().map(from_known_type).collect())
        }
        KnownType::StructuralRecord(fields) => {
            let mut row = Type::RowEmpty;
            for (name, value) in fields.into_iter().rev() {
                row = Type::RowExtend(name, Box::new(from_known_type(value)), Box::new(row));
            }
            Type::StructuralRecord(Box::new(row))
        }
        KnownType::Named(name) => Type::Named(name),
    }
}

fn parse_schema_type_annotation(text: &str, type_params: &[String]) -> Type {
    let text = text.trim();
    if text.is_empty() {
        return Type::Dynamic;
    }
    if text == "*" {
        return Type::Dynamic;
    }
    if text.starts_with('\'') && !text.contains('<') {
        return Type::Generic(text.to_string());
    }
    if let Some((params, result)) = split_function_type(text) {
        let params = if params.starts_with('(') && params.ends_with(')') {
            split_top_level(&params[1..params.len() - 1], ',')
                .into_iter()
                .map(|part| parse_schema_type_annotation(&part, type_params))
                .collect()
        } else {
            vec![parse_schema_type_annotation(&params, type_params)]
        };
        return Type::Function(
            params,
            Box::new(parse_schema_type_annotation(&result, type_params)),
        );
    }
    if let Some(inner) = text.strip_prefix("List<").and_then(|s| s.strip_suffix('>')) {
        return Type::List(Box::new(parse_schema_type_annotation(inner, type_params)));
    }
    if let Some(inner) = text.strip_prefix("Set<").and_then(|s| s.strip_suffix('>')) {
        return Type::Set(Box::new(parse_schema_type_annotation(inner, type_params)));
    }
    if let Some(inner) = text.strip_prefix("Map<").and_then(|s| s.strip_suffix('>')) {
        let parts = split_top_level(inner, ',');
        if parts.len() == 2 {
            return Type::Map(
                Box::new(parse_schema_type_annotation(&parts[0], type_params)),
                Box::new(parse_schema_type_annotation(&parts[1], type_params)),
            );
        }
    }
    if let Some((head, args)) = parse_type_application(text) {
        return Type::Applied(
            Box::new(parse_schema_type_annotation(&head, type_params)),
            args.into_iter()
                .map(|arg| parse_schema_type_annotation(&arg, type_params))
                .collect(),
        );
    }
    if let Some(name) = text.strip_prefix('#') {
        let name = name.trim();
        if let Some((record_name, args)) = parse_named_schema_args(name, type_params) {
            return Type::Record(record_name, args);
        }
        return Type::Record(name.to_string(), Vec::new());
    }
    match text {
        "Byte" => Type::Byte,
        "Short" => Type::Short,
        "Int" => Type::Int,
        "Long" => Type::Long,
        "Float" => Type::Float,
        "Double" => Type::Double,
        "Boolean" => Type::Bool,
        "Prop" => Type::Prop,
        "String" => Type::String,
        "Unit" => Type::Unit,
        other => Type::Named(other.to_string()),
    }
}

fn parse_type_annotation_with_named_vars(
    text: &str,
    named: &mut HashMap<String, Type>,
    next_var: &mut u32,
) -> Type {
    let text = text.trim();
    if text.is_empty() {
        return Type::Dynamic;
    }
    if text == "*" {
        return Type::Dynamic;
    }
    if let Some(inner) = text
        .strip_prefix("record")
        .map(str::trim)
        .filter(|inner| inner.starts_with('{') && inner.ends_with('}'))
    {
        return Type::StructuralRecord(Box::new(parse_structural_row(
            &inner[1..inner.len() - 1],
            named,
            next_var,
        )));
    }
    if text.starts_with('{') && text.ends_with('}') {
        return Type::StructuralRecord(Box::new(parse_structural_row(
            &text[1..text.len() - 1],
            named,
            next_var,
        )));
    }
    if text.starts_with('\'') && !text.contains('<') {
        return named
            .entry(text.to_string())
            .or_insert_with(|| {
                let id = *next_var;
                *next_var += 1;
                Type::Var(id)
            })
            .clone();
    }
    if let Some((params, result)) = split_function_type(text) {
        let params = if params.starts_with('(') && params.ends_with(')') {
            split_top_level(&params[1..params.len() - 1], ',')
                .into_iter()
                .map(|part| parse_type_annotation_with_named_vars(&part, named, next_var))
                .collect()
        } else {
            vec![parse_type_annotation_with_named_vars(
                &params, named, next_var,
            )]
        };
        return Type::Function(
            params,
            Box::new(parse_type_annotation_with_named_vars(
                &result, named, next_var,
            )),
        );
    }
    if let Some(inner) = text.strip_prefix("List<").and_then(|s| s.strip_suffix('>')) {
        return Type::List(Box::new(parse_type_annotation_with_named_vars(
            inner, named, next_var,
        )));
    }
    if let Some(inner) = text.strip_prefix("Set<").and_then(|s| s.strip_suffix('>')) {
        return Type::Set(Box::new(parse_type_annotation_with_named_vars(
            inner, named, next_var,
        )));
    }
    if let Some(inner) = text.strip_prefix("Map<").and_then(|s| s.strip_suffix('>')) {
        let parts = split_top_level(inner, ',');
        if parts.len() == 2 {
            return Type::Map(
                Box::new(parse_type_annotation_with_named_vars(
                    &parts[0], named, next_var,
                )),
                Box::new(parse_type_annotation_with_named_vars(
                    &parts[1], named, next_var,
                )),
            );
        }
    }
    if let Some((head, args)) = parse_type_application(text) {
        return Type::Applied(
            Box::new(parse_type_annotation_with_named_vars(
                &head, named, next_var,
            )),
            args.into_iter()
                .map(|arg| parse_type_annotation_with_named_vars(&arg, named, next_var))
                .collect(),
        );
    }
    if let Some(name) = text.strip_prefix('#') {
        let name = name.trim();
        if let Some((record_name, args)) = parse_named_generic_args(name, named, next_var) {
            return Type::Record(record_name, args);
        }
        return Type::Record(name.to_string(), Vec::new());
    }
    match text {
        "Byte" => Type::Byte,
        "Short" => Type::Short,
        "Int" => Type::Int,
        "Long" => Type::Long,
        "Float" => Type::Float,
        "Double" => Type::Double,
        "Boolean" => Type::Bool,
        "Prop" => Type::Prop,
        "String" => Type::String,
        "Unit" => Type::Unit,
        other => {
            // Identifiers pre-registered in `named` are treated as
            // type variables for this annotation context — extension
            // declarations seed `named` with their `<type-params>`
            // header so a generic `extension <a>(this: List<a>)`
            // unifies `a` consistently across the receiver and the
            // method bodies.
            if let Some(ty) = named.get(other) {
                return ty.clone();
            }
            Type::Named(other.to_string())
        }
    }
}

fn split_function_type(text: &str) -> Option<(String, String)> {
    let mut paren = 0usize;
    let mut angle = 0usize;
    let chars = text.char_indices().collect::<Vec<_>>();
    let mut index = 0usize;
    while index + 1 < chars.len() {
        let (_, ch) = chars[index];
        match ch {
            '(' => paren += 1,
            ')' => paren = paren.saturating_sub(1),
            '<' => angle += 1,
            '>' => angle = angle.saturating_sub(1),
            '=' if paren == 0 && angle == 0 && chars[index + 1].1 == '>' => {
                let left = text[..chars[index].0].trim().to_string();
                let right = text[chars[index + 1].0 + 1..].trim().to_string();
                return Some((left, right));
            }
            _ => {}
        }
        index += 1;
    }
    None
}

fn split_top_level(text: &str, separator: char) -> Vec<String> {
    let mut parts = Vec::new();
    let mut start = 0usize;
    let mut paren = 0usize;
    let mut angle = 0usize;
    for (index, ch) in text.char_indices() {
        match ch {
            '(' => paren += 1,
            ')' => paren = paren.saturating_sub(1),
            '<' => angle += 1,
            '>' => angle = angle.saturating_sub(1),
            _ if ch == separator && paren == 0 && angle == 0 => {
                parts.push(text[start..index].trim().to_string());
                start = index + ch.len_utf8();
            }
            _ => {}
        }
    }
    if start < text.len() {
        parts.push(text[start..].trim().to_string());
    }
    parts.into_iter().filter(|part| !part.is_empty()).collect()
}

fn parse_named_generic_args(
    text: &str,
    named: &mut HashMap<String, Type>,
    next_var: &mut u32,
) -> Option<(String, Vec<Type>)> {
    let name_end = text.find('<')?;
    let name = text[..name_end].trim().to_string();
    let inner = text[name_end + 1..].strip_suffix('>')?;
    let args = split_top_level(inner, ',')
        .into_iter()
        .map(|part| parse_type_annotation_with_named_vars(&part, named, next_var))
        .collect();
    Some((name, args))
}

fn parse_named_schema_args(text: &str, type_params: &[String]) -> Option<(String, Vec<Type>)> {
    let name_end = text.find('<')?;
    let name = text[..name_end].trim().to_string();
    let inner = text[name_end + 1..].strip_suffix('>')?;
    let args = split_top_level(inner, ',')
        .into_iter()
        .map(|part| parse_schema_type_annotation(&part, type_params))
        .collect();
    Some((name, args))
}

fn parse_type_application(text: &str) -> Option<(String, Vec<String>)> {
    let text = text.trim();
    let name_end = text.find('<')?;
    if !text.ends_with('>') {
        return None;
    }
    let head = text[..name_end].trim();
    if head.is_empty() || head.starts_with('#') {
        return None;
    }
    let inner = text[name_end + 1..].strip_suffix('>')?;
    let args = split_top_level(inner, ',');
    if args.is_empty() {
        return None;
    }
    Some((head.to_string(), args))
}

fn parse_structural_row(text: &str, named: &mut HashMap<String, Type>, next_var: &mut u32) -> Type {
    let mut row = Type::RowEmpty;
    let mut fields = split_top_level_multi(text, &[',', ';']);
    fields.retain(|field| !field.trim().is_empty());
    for field in fields.into_iter().rev() {
        let Some((name, annotation)) = split_label_annotation(&field) else {
            return Type::Dynamic;
        };
        row = Type::RowExtend(
            name,
            Box::new(parse_type_annotation_with_named_vars(
                &annotation,
                named,
                next_var,
            )),
            Box::new(row),
        );
    }
    row
}

fn split_top_level_multi(text: &str, separators: &[char]) -> Vec<String> {
    let mut parts = Vec::new();
    let mut start = 0usize;
    let mut paren = 0usize;
    let mut angle = 0usize;
    let mut brace = 0usize;
    for (index, ch) in text.char_indices() {
        match ch {
            '(' => paren += 1,
            ')' => paren = paren.saturating_sub(1),
            '<' => angle += 1,
            '>' => angle = angle.saturating_sub(1),
            '{' => brace += 1,
            '}' => brace = brace.saturating_sub(1),
            _ if separators.contains(&ch) && paren == 0 && angle == 0 && brace == 0 => {
                parts.push(text[start..index].trim().to_string());
                start = index + ch.len_utf8();
            }
            _ => {}
        }
    }
    if start < text.len() {
        parts.push(text[start..].trim().to_string());
    }
    parts
}

fn split_label_annotation(text: &str) -> Option<(String, String)> {
    let mut paren = 0usize;
    let mut angle = 0usize;
    let mut brace = 0usize;
    for (index, ch) in text.char_indices() {
        match ch {
            '(' => paren += 1,
            ')' => paren = paren.saturating_sub(1),
            '<' => angle += 1,
            '>' => angle = angle.saturating_sub(1),
            '{' => brace += 1,
            '}' => brace = brace.saturating_sub(1),
            ':' if paren == 0 && angle == 0 && brace == 0 => {
                return Some((
                    text[..index].trim().to_string(),
                    text[index + 1..].trim().to_string(),
                ));
            }
            _ => {}
        }
    }
    None
}

fn substitute_generics(ty: &Type, substitutions: &HashMap<String, Type>) -> Type {
    match ty {
        Type::Generic(name) => substitutions.get(name).cloned().unwrap_or(Type::Dynamic),
        Type::List(inner) => Type::List(Box::new(substitute_generics(inner, substitutions))),
        Type::Map(key, value) => Type::Map(
            Box::new(substitute_generics(key, substitutions)),
            Box::new(substitute_generics(value, substitutions)),
        ),
        Type::Set(inner) => Type::Set(Box::new(substitute_generics(inner, substitutions))),
        Type::Function(params, result) => Type::Function(
            params
                .iter()
                .map(|param| substitute_generics(param, substitutions))
                .collect(),
            Box::new(substitute_generics(result, substitutions)),
        ),
        Type::Applied(head, args) => {
            let head = substitute_generics(head, substitutions);
            let args = args
                .iter()
                .map(|arg| substitute_generics(arg, substitutions))
                .collect::<Vec<_>>();
            match (&head, args.as_slice()) {
                (Type::Named(name), [inner]) if name == "List" => {
                    Type::List(Box::new(inner.clone()))
                }
                (Type::Named(name), [inner]) if name == "Set" => Type::Set(Box::new(inner.clone())),
                (Type::Named(name), [key, value]) if name == "Map" => {
                    Type::Map(Box::new(key.clone()), Box::new(value.clone()))
                }
                _ => Type::Applied(Box::new(head), args),
            }
        }
        Type::Record(name, args) => Type::Record(
            name.clone(),
            args.iter()
                .map(|arg| substitute_generics(arg, substitutions))
                .collect(),
        ),
        Type::StructuralRecord(row) => {
            Type::StructuralRecord(Box::new(substitute_generics(row, substitutions)))
        }
        Type::RowExtend(label, field, rest) => Type::RowExtend(
            label.clone(),
            Box::new(substitute_generics(field, substitutions)),
            Box::new(substitute_generics(rest, substitutions)),
        ),
        other => other.clone(),
    }
}

fn replace_generic_vars(ty: &Type, replacements: &HashMap<GenericVar, Type>) -> Type {
    match ty {
        Type::Var(id) => replacements
            .get(&GenericVar::Type(*id))
            .cloned()
            .unwrap_or(Type::Var(*id)),
        Type::RowVar(id) => replacements
            .get(&GenericVar::Row(*id))
            .cloned()
            .unwrap_or(Type::RowVar(*id)),
        Type::List(inner) => Type::List(Box::new(replace_generic_vars(inner, replacements))),
        Type::Map(key, value) => Type::Map(
            Box::new(replace_generic_vars(key, replacements)),
            Box::new(replace_generic_vars(value, replacements)),
        ),
        Type::Set(inner) => Type::Set(Box::new(replace_generic_vars(inner, replacements))),
        Type::Function(params, result) => Type::Function(
            params
                .iter()
                .map(|param| replace_generic_vars(param, replacements))
                .collect(),
            Box::new(replace_generic_vars(result, replacements)),
        ),
        Type::Applied(head, args) => Type::Applied(
            Box::new(replace_generic_vars(head, replacements)),
            args.iter()
                .map(|arg| replace_generic_vars(arg, replacements))
                .collect(),
        ),
        Type::Record(name, args) => Type::Record(
            name.clone(),
            args.iter()
                .map(|arg| replace_generic_vars(arg, replacements))
                .collect(),
        ),
        Type::StructuralRecord(row) => {
            Type::StructuralRecord(Box::new(replace_generic_vars(row, replacements)))
        }
        Type::RowExtend(label, field, rest) => Type::RowExtend(
            label.clone(),
            Box::new(replace_generic_vars(field, replacements)),
            Box::new(replace_generic_vars(rest, replacements)),
        ),
        other => other.clone(),
    }
}

fn replace_constraint_generics(
    constraint: &Constraint,
    replacements: &HashMap<GenericVar, Type>,
) -> Constraint {
    Constraint {
        class_name: constraint.class_name.clone(),
        arguments: constraint
            .arguments
            .iter()
            .map(|argument| replace_generic_vars(argument, replacements))
            .collect(),
    }
}

fn match_constraint_pattern(
    pattern: &Constraint,
    actual: &Constraint,
) -> Option<HashMap<GenericVar, Type>> {
    if pattern.class_name != actual.class_name || pattern.arguments.len() != actual.arguments.len()
    {
        return None;
    }
    let mut replacements = HashMap::new();
    for (expected, actual) in pattern.arguments.iter().zip(actual.arguments.iter()) {
        if !match_type_pattern(expected, actual, &mut replacements) {
            return None;
        }
    }
    Some(replacements)
}

fn match_type_pattern(
    pattern: &Type,
    actual: &Type,
    replacements: &mut HashMap<GenericVar, Type>,
) -> bool {
    match pattern {
        Type::Var(id) => match replacements.get(&GenericVar::Type(*id)) {
            Some(bound) => bound == actual,
            None => {
                replacements.insert(GenericVar::Type(*id), actual.clone());
                true
            }
        },
        Type::RowVar(id) => match replacements.get(&GenericVar::Row(*id)) {
            Some(bound) => bound == actual,
            None => {
                replacements.insert(GenericVar::Row(*id), actual.clone());
                true
            }
        },
        Type::Dynamic | Type::Generic(_) => true,
        Type::Byte => matches!(actual, Type::Byte),
        Type::Short => matches!(actual, Type::Short),
        Type::Int => matches!(actual, Type::Int),
        Type::Long => matches!(actual, Type::Long),
        Type::Float => matches!(actual, Type::Float),
        Type::Double => matches!(actual, Type::Double),
        Type::Bool => matches!(actual, Type::Bool | Type::Prop),
        Type::Prop => matches!(actual, Type::Bool | Type::Prop),
        Type::String => matches!(actual, Type::String),
        Type::Unit => matches!(actual, Type::Unit),
        Type::Null => matches!(actual, Type::Null),
        Type::Named(expected) => matches!(actual, Type::Named(name) if name == expected),
        Type::List(expected) => {
            matches!(actual, Type::List(inner) if match_type_pattern(expected, inner, replacements))
        }
        Type::Set(expected) => {
            matches!(actual, Type::Set(inner) if match_type_pattern(expected, inner, replacements))
        }
        Type::Map(expected_key, expected_value) => matches!(
            actual,
            Type::Map(actual_key, actual_value)
                if match_type_pattern(expected_key, actual_key, replacements)
                    && match_type_pattern(expected_value, actual_value, replacements)
        ),
        Type::Function(expected_params, expected_result) => matches!(
            actual,
            Type::Function(actual_params, actual_result)
                if expected_params.len() == actual_params.len()
                    && expected_params
                        .iter()
                        .zip(actual_params.iter())
                        .all(|(expected, actual)| match_type_pattern(expected, actual, replacements))
                    && match_type_pattern(expected_result, actual_result, replacements)
        ),
        Type::Applied(expected_head, expected_args) => matches!(
            actual,
            Type::Applied(actual_head, actual_args)
                if match_type_pattern(expected_head, actual_head, replacements)
                    && expected_args.len() == actual_args.len()
                    && expected_args
                        .iter()
                        .zip(actual_args.iter())
                        .all(|(expected, actual)| match_type_pattern(expected, actual, replacements))
        ),
        Type::Record(expected_name, expected_args) => matches!(
            actual,
            Type::Record(actual_name, actual_args)
                if expected_name == actual_name
                    && expected_args.len() == actual_args.len()
                    && expected_args
                        .iter()
                        .zip(actual_args.iter())
                        .all(|(expected, actual)| match_type_pattern(expected, actual, replacements))
        ),
        Type::StructuralRecord(expected) => matches!(
            actual,
            Type::StructuralRecord(actual_row)
                if match_type_pattern(expected, actual_row, replacements)
        ),
        Type::RowEmpty => matches!(actual, Type::RowEmpty),
        Type::RowExtend(expected_label, expected_field, expected_rest) => matches!(
            actual,
            Type::RowExtend(actual_label, actual_field, actual_rest)
                if expected_label == actual_label
                    && match_type_pattern(expected_field, actual_field, replacements)
                    && match_type_pattern(expected_rest, actual_rest, replacements)
        ),
    }
}

fn free_vars(ty: &Type) -> Vec<GenericVar> {
    let mut vars = Vec::new();
    collect_free_vars(ty, &mut vars);
    vars
}

fn free_vars_in_constraints(constraints: &[Constraint]) -> Vec<GenericVar> {
    let mut vars = Vec::new();
    for constraint in constraints {
        for argument in &constraint.arguments {
            collect_free_vars(argument, &mut vars);
        }
    }
    vars
}

fn collect_free_vars(ty: &Type, vars: &mut Vec<GenericVar>) {
    match ty {
        Type::Var(id) => push_unique(vars, GenericVar::Type(*id)),
        Type::RowVar(id) => push_unique(vars, GenericVar::Row(*id)),
        Type::List(inner) | Type::Set(inner) | Type::StructuralRecord(inner) => {
            collect_free_vars(inner, vars)
        }
        Type::Map(key, value) => {
            collect_free_vars(key, vars);
            collect_free_vars(value, vars);
        }
        Type::Function(params, result) => {
            for param in params {
                collect_free_vars(param, vars);
            }
            collect_free_vars(result, vars);
        }
        Type::Applied(head, args) => {
            collect_free_vars(head, vars);
            for arg in args {
                collect_free_vars(arg, vars);
            }
        }
        Type::Record(_, args) => {
            for arg in args {
                collect_free_vars(arg, vars);
            }
        }
        Type::RowExtend(_, field, rest) => {
            collect_free_vars(field, vars);
            collect_free_vars(rest, vars);
        }
        _ => {}
    }
}

fn occurs_in(var: GenericVar, ty: &Type) -> bool {
    match ty {
        Type::Var(id) => var == GenericVar::Type(*id),
        Type::RowVar(id) => var == GenericVar::Row(*id),
        Type::List(inner) | Type::Set(inner) | Type::StructuralRecord(inner) => {
            occurs_in(var, inner)
        }
        Type::Map(key, value) => occurs_in(var.clone(), key) || occurs_in(var, value),
        Type::Function(params, result) => {
            params.iter().any(|param| occurs_in(var.clone(), param)) || occurs_in(var, result)
        }
        Type::Applied(head, args) => {
            occurs_in(var.clone(), head) || args.iter().any(|arg| occurs_in(var.clone(), arg))
        }
        Type::Record(_, args) => args.iter().any(|arg| occurs_in(var.clone(), arg)),
        Type::RowExtend(_, field, rest) => occurs_in(var.clone(), field) || occurs_in(var, rest),
        _ => false,
    }
}

fn is_row_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::RowEmpty | Type::RowExtend(_, _, _) | Type::RowVar(_)
    )
}

fn push_unique(vars: &mut Vec<GenericVar>, var: GenericVar) {
    if !vars.contains(&var) {
        vars.push(var);
    }
}

fn display_type(ty: &Type) -> String {
    match ty {
        Type::Byte => "Byte".to_string(),
        Type::Short => "Short".to_string(),
        Type::Int => "Int".to_string(),
        Type::Long => "Long".to_string(),
        Type::Float => "Float".to_string(),
        Type::Double => "Double".to_string(),
        Type::Bool => "Boolean".to_string(),
        Type::Prop => "Prop".to_string(),
        Type::String => "String".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Dynamic => "*".to_string(),
        Type::Null => "null".to_string(),
        Type::List(inner) => format!("List<{}>", display_type(inner)),
        Type::Map(key, value) => format!("Map<{}, {}>", display_type(key), display_type(value)),
        Type::Set(inner) => format!("Set<{}>", display_type(inner)),
        Type::Function(params, result) => format!(
            "({}) => {}",
            params
                .iter()
                .map(display_type)
                .collect::<Vec<_>>()
                .join(", "),
            display_type(result)
        ),
        Type::Applied(head, args) => format!(
            "{}<{}>",
            display_type(head),
            args.iter().map(display_type).collect::<Vec<_>>().join(", ")
        ),
        Type::Record(name, args) if args.is_empty() => format!("#{name}"),
        Type::Record(name, args) => format!(
            "#{}<{}>",
            name,
            args.iter().map(display_type).collect::<Vec<_>>().join(", ")
        ),
        Type::StructuralRecord(row) => format!("record {{ {} }}", display_row(row)),
        Type::RowEmpty => String::new(),
        Type::RowExtend(_, _, _) => display_row(ty),
        Type::Generic(name) => name.clone(),
        Type::Named(name) => name.clone(),
        Type::Var(id) => format!("'t{id}"),
        Type::RowVar(id) => format!("'r{id}"),
    }
}

fn display_row(ty: &Type) -> String {
    match ty {
        Type::RowEmpty => String::new(),
        Type::RowVar(id) => format!("... 'r{id}"),
        Type::RowExtend(label, field, rest) => {
            let mut parts = vec![format!("{label}: {}", display_type(field))];
            let rest = display_row(rest);
            if !rest.is_empty() {
                parts.push(rest);
            }
            parts.join("; ")
        }
        other => display_type(other),
    }
}

fn substitute_expr_identifiers(expr: &Expr, substitutions: &HashMap<String, Expr>) -> Expr {
    match expr {
        Expr::Int { .. }
        | Expr::Double { .. }
        | Expr::Bool { .. }
        | Expr::String { .. }
        | Expr::Null { .. }
        | Expr::Unit { .. }
        | Expr::ModuleHeader { .. }
        | Expr::Import { .. }
        | Expr::ExtensionDeclaration { .. }
        | Expr::EnumDeclaration { .. }
        | Expr::Match { .. }
        | Expr::PegRuleBlock { .. } => expr.clone(),
        Expr::Identifier { name, .. } => substitutions
            .get(name)
            .cloned()
            .unwrap_or_else(|| expr.clone()),
        Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span,
        } => Expr::RecordDeclaration {
            name: name.clone(),
            type_params: type_params.clone(),
            fields: fields.clone(),
            span: *span,
        },
        Expr::RecordLiteral { fields, span } => Expr::RecordLiteral {
            fields: fields
                .iter()
                .map(|(name, value)| {
                    (
                        name.clone(),
                        substitute_expr_identifiers(value, substitutions),
                    )
                })
                .collect(),
            span: *span,
        },
        Expr::TypeClassDeclaration {
            name,
            type_params,
            methods,
            span,
        } => Expr::TypeClassDeclaration {
            name: name.clone(),
            type_params: type_params.clone(),
            methods: methods.clone(),
            span: *span,
        },
        Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation,
            constraints,
            methods,
            span,
        } => Expr::InstanceDeclaration {
            class_name: class_name.clone(),
            for_type: for_type.clone(),
            for_type_annotation: for_type_annotation.clone(),
            constraints: constraints.clone(),
            methods: methods
                .iter()
                .map(|method| substitute_expr_identifiers(method, substitutions))
                .collect(),
            span: *span,
        },
        Expr::TheoremDeclaration {
            name,
            params,
            param_annotations,
            proposition,
            body,
            trusted,
            span,
        } => Expr::TheoremDeclaration {
            name: name.clone(),
            params: params.clone(),
            param_annotations: param_annotations.clone(),
            proposition: Box::new(substitute_expr_identifiers(proposition, substitutions)),
            body: Box::new(substitute_expr_identifiers(body, substitutions)),
            trusted: *trusted,
            span: *span,
        },
        Expr::AxiomDeclaration {
            name,
            params,
            param_annotations,
            proposition,
            span,
        } => Expr::AxiomDeclaration {
            name: name.clone(),
            params: params.clone(),
            param_annotations: param_annotations.clone(),
            proposition: Box::new(substitute_expr_identifiers(proposition, substitutions)),
            span: *span,
        },
        Expr::VarDecl {
            mutable,
            name,
            annotation,
            value,
            span,
        } => Expr::VarDecl {
            mutable: *mutable,
            name: name.clone(),
            annotation: annotation.clone(),
            value: Box::new(substitute_expr_identifiers(value, substitutions)),
            span: *span,
        },
        Expr::DefDecl {
            name,
            type_params,
            constraints,
            params,
            param_annotations,
            return_annotation,
            body,
            span,
        } => Expr::DefDecl {
            name: name.clone(),
            type_params: type_params.clone(),
            constraints: constraints.clone(),
            params: params.clone(),
            param_annotations: param_annotations.clone(),
            return_annotation: return_annotation.clone(),
            body: Box::new(substitute_expr_identifiers(body, substitutions)),
            span: *span,
        },
        Expr::Lambda {
            params,
            param_annotations,
            body,
            span,
        } => Expr::Lambda {
            params: params.clone(),
            param_annotations: param_annotations.clone(),
            body: Box::new(substitute_expr_identifiers(body, substitutions)),
            span: *span,
        },
        Expr::Assign { name, value, span } => Expr::Assign {
            name: name.clone(),
            value: Box::new(substitute_expr_identifiers(value, substitutions)),
            span: *span,
        },
        Expr::Unary { op, expr, span } => Expr::Unary {
            op: *op,
            expr: Box::new(substitute_expr_identifiers(expr, substitutions)),
            span: *span,
        },
        Expr::Binary { lhs, op, rhs, span } => Expr::Binary {
            lhs: Box::new(substitute_expr_identifiers(lhs, substitutions)),
            op: *op,
            rhs: Box::new(substitute_expr_identifiers(rhs, substitutions)),
            span: *span,
        },
        Expr::Call {
            callee,
            arguments,
            span,
        } => Expr::Call {
            callee: Box::new(substitute_expr_identifiers(callee, substitutions)),
            arguments: arguments
                .iter()
                .map(|argument| substitute_expr_identifiers(argument, substitutions))
                .collect(),
            span: *span,
        },
        Expr::FieldAccess {
            target,
            field,
            span,
        } => Expr::FieldAccess {
            target: Box::new(substitute_expr_identifiers(target, substitutions)),
            field: field.clone(),
            span: *span,
        },
        Expr::Cleanup {
            body,
            cleanup,
            span,
        } => Expr::Cleanup {
            body: Box::new(substitute_expr_identifiers(body, substitutions)),
            cleanup: Box::new(substitute_expr_identifiers(cleanup, substitutions)),
            span: *span,
        },
        Expr::RecordConstructor {
            name,
            arguments,
            span,
        } => Expr::RecordConstructor {
            name: name.clone(),
            arguments: arguments
                .iter()
                .map(|argument| substitute_expr_identifiers(argument, substitutions))
                .collect(),
            span: *span,
        },
        Expr::ListLiteral { elements, span } => Expr::ListLiteral {
            elements: elements
                .iter()
                .map(|element| substitute_expr_identifiers(element, substitutions))
                .collect(),
            span: *span,
        },
        Expr::MapLiteral { entries, span } => Expr::MapLiteral {
            entries: entries
                .iter()
                .map(|(key, value)| {
                    (
                        substitute_expr_identifiers(key, substitutions),
                        substitute_expr_identifiers(value, substitutions),
                    )
                })
                .collect(),
            span: *span,
        },
        Expr::SetLiteral { elements, span } => Expr::SetLiteral {
            elements: elements
                .iter()
                .map(|element| substitute_expr_identifiers(element, substitutions))
                .collect(),
            span: *span,
        },
        Expr::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => Expr::If {
            condition: Box::new(substitute_expr_identifiers(condition, substitutions)),
            then_branch: Box::new(substitute_expr_identifiers(then_branch, substitutions)),
            else_branch: else_branch
                .as_ref()
                .map(|branch| Box::new(substitute_expr_identifiers(branch, substitutions))),
            span: *span,
        },
        Expr::While {
            condition,
            body,
            span,
        } => Expr::While {
            condition: Box::new(substitute_expr_identifiers(condition, substitutions)),
            body: Box::new(substitute_expr_identifiers(body, substitutions)),
            span: *span,
        },
        Expr::Foreach {
            binding,
            iterable,
            body,
            span,
        } => Expr::Foreach {
            binding: binding.clone(),
            iterable: Box::new(substitute_expr_identifiers(iterable, substitutions)),
            body: Box::new(substitute_expr_identifiers(body, substitutions)),
            span: *span,
        },
        Expr::Block { expressions, span } => Expr::Block {
            expressions: expressions
                .iter()
                .map(|expression| substitute_expr_identifiers(expression, substitutions))
                .collect(),
            span: *span,
        },
    }
}

fn expr_equivalent_ignoring_spans(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (
            Expr::Int {
                value: lv,
                kind: lk,
                ..
            },
            Expr::Int {
                value: rv,
                kind: rk,
                ..
            },
        ) => lv == rv && lk == rk,
        (
            Expr::Double {
                value: lv,
                kind: lk,
                ..
            },
            Expr::Double {
                value: rv,
                kind: rk,
                ..
            },
        ) => lv.to_bits() == rv.to_bits() && lk == rk,
        (Expr::Bool { value: lv, .. }, Expr::Bool { value: rv, .. }) => lv == rv,
        (Expr::String { value: lv, .. }, Expr::String { value: rv, .. }) => lv == rv,
        (Expr::Null { .. }, Expr::Null { .. }) | (Expr::Unit { .. }, Expr::Unit { .. }) => true,
        (Expr::Identifier { name: ln, .. }, Expr::Identifier { name: rn, .. }) => ln == rn,
        (Expr::ModuleHeader { name: ln, .. }, Expr::ModuleHeader { name: rn, .. }) => ln == rn,
        (
            Expr::Import {
                path: lp,
                alias: la,
                members: lm,
                excludes: le,
                ..
            },
            Expr::Import {
                path: rp,
                alias: ra,
                members: rm,
                excludes: re,
                ..
            },
        ) => lp == rp && la == ra && lm == rm && le == re,
        (
            Expr::RecordDeclaration {
                name: ln,
                type_params: ltp,
                fields: lf,
                ..
            },
            Expr::RecordDeclaration {
                name: rn,
                type_params: rtp,
                fields: rf,
                ..
            },
        ) => ln == rn && ltp == rtp && lf == rf,
        (Expr::RecordLiteral { fields: lf, .. }, Expr::RecordLiteral { fields: rf, .. }) => {
            lf.len() == rf.len()
                && lf
                    .iter()
                    .zip(rf.iter())
                    .all(|((ln, lv), (rn, rv))| ln == rn && expr_equivalent_ignoring_spans(lv, rv))
        }
        (
            Expr::TypeClassDeclaration {
                name: ln,
                type_params: ltp,
                methods: lm,
                ..
            },
            Expr::TypeClassDeclaration {
                name: rn,
                type_params: rtp,
                methods: rm,
                ..
            },
        ) => ln == rn && ltp == rtp && lm == rm,
        (
            Expr::InstanceDeclaration {
                class_name: lc,
                for_type: lft,
                for_type_annotation: lfa,
                constraints: lcs,
                methods: lm,
                ..
            },
            Expr::InstanceDeclaration {
                class_name: rc,
                for_type: rft,
                for_type_annotation: rfa,
                constraints: rcs,
                methods: rm,
                ..
            },
        ) => lc == rc && lft == rft && lfa == rfa && lcs == rcs && vec_exprs_equivalent(lm, rm),
        (
            Expr::TheoremDeclaration {
                name: ln,
                params: lp,
                param_annotations: lpa,
                proposition: lprop,
                body: lb,
                trusted: lt,
                ..
            },
            Expr::TheoremDeclaration {
                name: rn,
                params: rp,
                param_annotations: rpa,
                proposition: rprop,
                body: rb,
                trusted: rt,
                ..
            },
        ) => {
            ln == rn
                && lp == rp
                && lpa == rpa
                && lt == rt
                && expr_equivalent_ignoring_spans(lprop, rprop)
                && expr_equivalent_ignoring_spans(lb, rb)
        }
        (
            Expr::AxiomDeclaration {
                name: ln,
                params: lp,
                param_annotations: lpa,
                proposition: lprop,
                ..
            },
            Expr::AxiomDeclaration {
                name: rn,
                params: rp,
                param_annotations: rpa,
                proposition: rprop,
                ..
            },
        ) => ln == rn && lp == rp && lpa == rpa && expr_equivalent_ignoring_spans(lprop, rprop),
        (Expr::PegRuleBlock { .. }, Expr::PegRuleBlock { .. }) => true,
        (
            Expr::VarDecl {
                mutable: lm,
                name: ln,
                annotation: la,
                value: lv,
                ..
            },
            Expr::VarDecl {
                mutable: rm,
                name: rn,
                annotation: ra,
                value: rv,
                ..
            },
        ) => lm == rm && ln == rn && la == ra && expr_equivalent_ignoring_spans(lv, rv),
        (
            Expr::DefDecl {
                name: ln,
                type_params: ltp,
                constraints: lc,
                params: lp,
                param_annotations: lpa,
                return_annotation: lra,
                body: lb,
                ..
            },
            Expr::DefDecl {
                name: rn,
                type_params: rtp,
                constraints: rc,
                params: rp,
                param_annotations: rpa,
                return_annotation: rra,
                body: rb,
                ..
            },
        ) => {
            ln == rn
                && ltp == rtp
                && lc == rc
                && lp == rp
                && lpa == rpa
                && lra == rra
                && expr_equivalent_ignoring_spans(lb, rb)
        }
        (
            Expr::Lambda {
                params: lp,
                param_annotations: lpa,
                body: lb,
                ..
            },
            Expr::Lambda {
                params: rp,
                param_annotations: rpa,
                body: rb,
                ..
            },
        ) => lp == rp && lpa == rpa && expr_equivalent_ignoring_spans(lb, rb),
        (
            Expr::Assign {
                name: ln,
                value: lv,
                ..
            },
            Expr::Assign {
                name: rn,
                value: rv,
                ..
            },
        ) => ln == rn && expr_equivalent_ignoring_spans(lv, rv),
        (
            Expr::Unary {
                op: lo, expr: le, ..
            },
            Expr::Unary {
                op: ro, expr: re, ..
            },
        ) => lo == ro && expr_equivalent_ignoring_spans(le, re),
        (
            Expr::Binary {
                lhs: ll,
                op: lo,
                rhs: lr,
                ..
            },
            Expr::Binary {
                lhs: rl,
                op: ro,
                rhs: rr,
                ..
            },
        ) => {
            lo == ro
                && expr_equivalent_ignoring_spans(ll, rl)
                && expr_equivalent_ignoring_spans(lr, rr)
        }
        (
            Expr::Call {
                callee: lc,
                arguments: la,
                ..
            },
            Expr::Call {
                callee: rc,
                arguments: ra,
                ..
            },
        ) => expr_equivalent_ignoring_spans(lc, rc) && vec_exprs_equivalent(la, ra),
        (
            Expr::FieldAccess {
                target: lt,
                field: lf,
                ..
            },
            Expr::FieldAccess {
                target: rt,
                field: rf,
                ..
            },
        ) => lf == rf && expr_equivalent_ignoring_spans(lt, rt),
        (
            Expr::Cleanup {
                body: lb,
                cleanup: lc,
                ..
            },
            Expr::Cleanup {
                body: rb,
                cleanup: rc,
                ..
            },
        ) => expr_equivalent_ignoring_spans(lb, rb) && expr_equivalent_ignoring_spans(lc, rc),
        (
            Expr::RecordConstructor {
                name: ln,
                arguments: la,
                ..
            },
            Expr::RecordConstructor {
                name: rn,
                arguments: ra,
                ..
            },
        ) => ln == rn && vec_exprs_equivalent(la, ra),
        (Expr::ListLiteral { elements: le, .. }, Expr::ListLiteral { elements: re, .. }) => {
            vec_exprs_equivalent(le, re)
        }
        (Expr::MapLiteral { entries: le, .. }, Expr::MapLiteral { entries: re, .. }) => {
            le.len() == re.len()
                && le.iter().zip(re.iter()).all(|((lk, lv), (rk, rv))| {
                    expr_equivalent_ignoring_spans(lk, rk) && expr_equivalent_ignoring_spans(lv, rv)
                })
        }
        (Expr::SetLiteral { elements: le, .. }, Expr::SetLiteral { elements: re, .. }) => {
            vec_exprs_equivalent(le, re)
        }
        (
            Expr::If {
                condition: lc,
                then_branch: lt,
                else_branch: le,
                ..
            },
            Expr::If {
                condition: rc,
                then_branch: rt,
                else_branch: re,
                ..
            },
        ) => {
            expr_equivalent_ignoring_spans(lc, rc)
                && expr_equivalent_ignoring_spans(lt, rt)
                && match (le, re) {
                    (Some(lb), Some(rb)) => expr_equivalent_ignoring_spans(lb, rb),
                    (None, None) => true,
                    _ => false,
                }
        }
        (
            Expr::While {
                condition: lc,
                body: lb,
                ..
            },
            Expr::While {
                condition: rc,
                body: rb,
                ..
            },
        ) => expr_equivalent_ignoring_spans(lc, rc) && expr_equivalent_ignoring_spans(lb, rb),
        (
            Expr::Foreach {
                binding: lb,
                iterable: li,
                body: lbody,
                ..
            },
            Expr::Foreach {
                binding: rb,
                iterable: ri,
                body: rbody,
                ..
            },
        ) => {
            lb == rb
                && expr_equivalent_ignoring_spans(li, ri)
                && expr_equivalent_ignoring_spans(lbody, rbody)
        }
        (
            Expr::Block {
                expressions: le, ..
            },
            Expr::Block {
                expressions: re, ..
            },
        ) => vec_exprs_equivalent(le, re),
        _ => false,
    }
}

fn vec_exprs_equivalent(left: &[Expr], right: &[Expr]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right.iter())
            .all(|(left, right)| expr_equivalent_ignoring_spans(left, right))
}

fn promote_numeric(lhs: Type, rhs: Type) -> Type {
    use Type::*;
    let rank = |ty: &Type| match ty {
        Byte => 0,
        Short => 1,
        Int => 2,
        Long => 3,
        Float => 4,
        Double => 5,
        _ => usize::MAX,
    };
    if rank(&lhs) >= rank(&rhs) { lhs } else { rhs }
}

fn type_error(span: Span, message: impl Into<String>) -> Diagnostic {
    Diagnostic {
        kind: DiagnosticKind::Type,
        severity: Severity::Error,
        span: Some(span),
        message: message.into(),
        incomplete_input: false,
    }
}

#[cfg(test)]
mod tests {
    use klassic_span::SourceFile;
    use klassic_syntax::parse_source;

    use super::typecheck_program;

    #[test]
    fn accepts_basic_function_annotations() {
        let source = SourceFile::new(
            "test.kl",
            "def add(x: Int, y: Int): Int = x + y\nval f: (Int, Int) => Int = add\nf(2, 3)",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("program should typecheck");
    }

    #[test]
    fn rejects_incompatible_annotation() {
        let source = SourceFile::new("test.kl", "val n: Int = \"hello\"");
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("annotation mismatch should fail");
        assert!(error.message.contains("type mismatch"));
    }

    #[test]
    fn rejects_assignment_to_val() {
        let source = SourceFile::new("test.kl", "val a = 1\na = 2\na");
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("assignment should fail");
        assert!(error.message.contains("immutable"));
    }

    #[test]
    fn rejects_partial_application_for_non_curried_functions() {
        let source = SourceFile::new("test.kl", "def f(x, y) = x + y\nf(10)");
        let expr = parse_source(&source).expect("program should parse");
        let error =
            typecheck_program(&expr).expect_err("non-curried partial application should fail");
        assert!(
            error
                .message
                .contains("function expects 2 arguments but got 1")
        );
    }

    #[test]
    fn rejects_record_constructor_with_wrong_field_type() {
        let source = SourceFile::new(
            "test.kl",
            "record Person {\n  name: *\n  age: Int\n}\nval p = #Person(\"Hoge\", 1.0)",
        );
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("record constructor mismatch should fail");
        assert!(error.message.contains("type mismatch"));
    }

    #[test]
    fn rejects_incompatible_record_field_use() {
        let source = SourceFile::new(
            "test.kl",
            "record Person {\n  name: *\n  age: Int\n}\nval p = #Person(\"Hoge\", 7)\nval k: Double = p.age",
        );
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("record field mismatch should fail");
        assert!(error.message.contains("type mismatch"));
    }

    #[test]
    fn excludes_hidden_import_members_from_unqualified_scope() {
        let module = SourceFile::new(
            "module.kl",
            "module hidden.test\ndef allowed() = 1\ndef hidden() = 2",
        );
        let module_expr = parse_source(&module).expect("module should parse");
        typecheck_program(&module_expr).expect("module should typecheck");
        crate::clear_user_binding_types();

        let source = SourceFile::new(
            "test.kl",
            "import hidden.test.{allowed, hidden => _}\nallowed()\nhidden()",
        );
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("hidden import should stay undefined");
        assert!(error.message.contains("undefined variable `hidden`"));
    }

    #[test]
    fn infers_generic_record_constructor_and_field_types() {
        let source = SourceFile::new(
            "test.kl",
            "record Pair<'a, 'b> {\n  _1: 'a\n  _2: 'b\n}\nval pair1: #Pair<Int, Int> = #Pair(1, 2)\nval left: Int = pair1._1\nval pair2 = #Pair(1.0, 2)\nval right: Double = pair2._1",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("generic record constructor inference should typecheck");
    }

    #[test]
    fn rejects_incompatible_generic_record_annotation() {
        let source = SourceFile::new(
            "test.kl",
            "record Pair<'a, 'b> {\n  _1: 'a\n  _2: 'b\n}\nval pair: #Pair<Int, Int> = #Pair(1.0, 2)",
        );
        let expr = parse_source(&source).expect("program should parse");
        let error =
            typecheck_program(&expr).expect_err("generic record annotation mismatch should fail");
        assert!(error.message.contains("type mismatch"));
    }

    #[test]
    fn rejects_mixed_integral_arithmetic() {
        let source = SourceFile::new("test.kl", "1 + 2L");
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("mixed integral arithmetic should fail");
        assert!(error.message.contains("type mismatch"));
    }

    #[test]
    fn rejects_undefined_variable_use() {
        let source = SourceFile::new("test.kl", "foreach(a in [1, 2, 3]) {\n  b + 3\n}");
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("undefined variable should fail");
        assert!(error.message.contains("undefined variable `b`"));
    }

    #[test]
    fn generalizes_immutable_lambda_bindings() {
        let source = SourceFile::new(
            "test.kl",
            "val id = (x) => x\nval a: Int = id(1)\nval b: String = id(\"ok\")",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("immutable binding should generalize");
    }

    #[test]
    fn infers_row_polymorphic_field_access_over_nominal_records() {
        let source = SourceFile::new(
            "test.kl",
            "record P {\n  x: Int\n  y: Int\n  z: Int\n}\nrecord T<'a, 'b> {\n  x: 'a\n  y: 'b\n}\ndef add_xy(o) = {\n  o.x + o.y\n}\nval a: Int = add_xy(#P(1, 2, 3))\nval b: Int = add_xy(#T(1, 2))",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("row-polymorphic field access should typecheck");
    }

    #[test]
    fn rejects_polymorphic_result_annotation_mismatch() {
        let source = SourceFile::new("test.kl", "val id = (x) => x\nval a: Int = id(\"oops\")");
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("annotation mismatch should fail");
        assert!(error.to_string().contains("type mismatch"));
    }

    #[test]
    fn generalizes_row_field_access_without_losing_field_types() {
        let source = SourceFile::new(
            "test.kl",
            "def get_x(o) = o.x\nval a: Int = get_x(record { x: 1, y: \"ok\" })\nval b: String = get_x(record { x: \"ok\", z: 2 })",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("row-polymorphic field result should generalize");
    }

    #[test]
    fn rejects_row_field_type_mismatch_after_numeric_use() {
        let source = SourceFile::new(
            "test.kl",
            "def increment_x(o) = o.x + 1\nval a: Int = increment_x(record { x: 1, y: \"ok\" })\nval b: Int = increment_x(record { x: \"bad\", z: 2 })",
        );
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("row field mismatch should fail");
        assert!(error.to_string().contains("type mismatch"));
    }

    #[test]
    fn infers_empty_list_accumulator_from_contextual_lambda_result() {
        let source = SourceFile::new(
            "test.kl",
            "val reversed: List<Int> = foldLeft([1, 2, 3])([])((acc, e) => e #cons acc)",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("empty accumulator should be inferred from reducer");
    }

    #[test]
    fn typechecks_structural_record_dictionary_annotations() {
        let source = SourceFile::new(
            "test.kl",
            "val show_int = record { show: (x: Int) => \"Int(\" + x + \")\" }\ndef show_value(dict: record { show: ('a) => String }, x: 'a): String = dict.show(x)\nval rendered: String = show_value(show_int, 42)",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("structural record annotations should typecheck");
    }

    #[test]
    fn typechecks_typeclass_constrained_polymorphic_functions() {
        let source = SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ntypeclass Eq<'a> where {\n  eq: ('a, 'a) => Boolean\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Eq<Int> where {\n  def eq(x: Int, y: Int): Boolean = x == y\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\ndef show_if_equal<'a>(x: 'a, y: 'a): String where (Show<'a>, Eq<'a>) = if(eq(x, y)) show(x) else show(y)\nval rendered: String = display(42)\nval also_rendered: String = show_if_equal(1, 2)",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("constrained polymorphic functions should typecheck");
    }

    #[test]
    fn rejects_constrained_calls_without_matching_instance() {
        let source = SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\ndisplay(\"nope\")",
        );
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("missing typeclass instance should fail");
        assert!(error.message.contains("missing instance for Show<String>"));
    }

    #[test]
    fn typechecks_constrained_generic_instances() {
        let source = SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Show<List<'a>> where Show<'a> {\n  def show(xs: List<'a>): String = \"list\"\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\nval rendered: String = display([1, 2, 3])",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("constrained generic instances should typecheck");
    }

    #[test]
    fn typechecks_repeated_constrained_calls_at_multiple_types() {
        let source = SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Show<String> where {\n  def show(x: String): String = \"Str(\" + x + \")\"\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\nval a: String = display(42)\nval b: String = display(\"hello\")",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr)
            .expect("constrained function should instantiate freshly on each call");
    }

    #[test]
    fn propagates_instance_constraints_through_generic_code() {
        let source = SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Show<List<'a>> where Show<'a> {\n  def show(xs: List<'a>): String = \"list\"\n}\ndef show_list<'a>(xs: List<'a>): String where Show<'a> = show(xs)\nval rendered: String = show_list([1, 2, 3])",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr)
            .expect("constraint solver should satisfy Show<List<'a>> from Show<'a>");
    }

    #[test]
    fn typechecks_higher_kinded_constrained_functions() {
        let source = SourceFile::new(
            "test.kl",
            "typeclass Functor<'f: * => *> where {\n  map: (('a) => 'b, 'f<'a>) => 'f<'b>\n}\ninstance Functor<List> where {\n  def map(f: ('a) => 'b, xs: List<'a>): List<'b> = xs.map(f)\n}\ndef liftTwice<'f, 'a, 'b, 'c>(xs: 'f<'a>, f: ('a) => 'b, g: ('b) => 'c): 'f<'c> where Functor<'f> = map(g, map(f, xs))\nval rendered = liftTwice([1, 2, 3], (x) => x + 1, (y) => y * 2)",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("higher-kinded constrained helper should typecheck");
    }

    #[test]
    fn typechecks_forward_proof_references_with_annotations() {
        let source = SourceFile::new(
            "test.kl",
            "theorem earlier(x: Int): { later(x) } = assert(true)\naxiom later(y: Int): { true }\nassert(earlier(1))",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("proofs should be hoisted and typechecked");
    }

    #[test]
    fn typechecks_matching_proof_term_bodies() {
        let source = SourceFile::new(
            "test.kl",
            "axiom base(): { true }\ntheorem mid(): { base } = base\nassert(mid)",
        );
        let expr = parse_source(&source).expect("program should parse");
        typecheck_program(&expr).expect("matching proof-term body should typecheck");
    }

    #[test]
    fn rejects_non_boolean_theorem_propositions() {
        let source = SourceFile::new("test.kl", "theorem bad(): { 1 } = assert(true)");
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("non-boolean proposition should fail");
        assert!(error.message.contains("type mismatch"));
    }

    #[test]
    fn rejects_non_unit_theorem_bodies() {
        let source = SourceFile::new("test.kl", "theorem bad(): { true } = 1");
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("non-unit proof body should fail");
        assert!(error.message.contains("type mismatch"));
    }

    #[test]
    fn rejects_mismatched_proof_term_bodies() {
        let source = SourceFile::new(
            "test.kl",
            "axiom left(): { true }\naxiom right(): { false }\ntheorem bad(): { left } = right",
        );
        let expr = parse_source(&source).expect("program should parse");
        let error = typecheck_program(&expr).expect_err("mismatched proof body should fail");
        assert!(
            error
                .message
                .contains("proof body does not establish declared proposition")
        );
    }
}
