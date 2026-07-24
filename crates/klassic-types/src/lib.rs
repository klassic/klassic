use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use klassic_span::{Diagnostic, DiagnosticKind, Severity, Span};
use klassic_syntax::{
    Expr, FloatLiteralKind, IntLiteralKind, RecordField, StringPart, TypeAnnotation,
    TypeClassConstraint, TypeClassMethod,
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
    /// A nominal algebraic data type instantiated at the given type
    /// arguments (`Option<Int>` = `Enum("Option", [Int])`). Kept apart
    /// from `Record` so structural-record interop and field-access
    /// rules never apply to sum values.
    Enum(String, Vec<Type>),
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

/// A declared algebraic data type: its type parameters and each
/// variant's field types, expressed over `Type::Generic` parameters.
#[derive(Clone, Debug)]
struct EnumSchema {
    type_params: Vec<String>,
    variants: Vec<(String, Vec<Type>)>,
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
    enum_schemas: HashMap<String, EnumSchema>,
    /// Tentative (params, return) types from block-level def
    /// predeclaration, consumed by the DefDecl inference so forward
    /// calls and the eventual definition share one set of variables.
    predeclared_defs: HashMap<String, (Vec<Type>, Type)>,
    typeclasses: HashMap<String, TypeClassInfo>,
    instances: Vec<InstanceInfo>,
    current_module: Option<String>,
    next_var: u32,
    substitutions: HashMap<GenericVar, Type>,
    /// Constraints inferred from arithmetic operators on still-unresolved
    /// type variables (built-in `Num` / `Plus` classes). They are drained
    /// into a `def`'s generalized signature, or defaulted to `Int` for
    /// vars that never become generalizable.
    inferred_constraints: Vec<Constraint>,
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
    static USER_ENUM_SCHEMAS: RefCell<HashMap<String, EnumSchema>> = RefCell::new(HashMap::new());
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
        Type::Record(name, _) | Type::Enum(name, _) | Type::Named(name) => name.clone(),
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
    typecheck_program_with_bindings_typed(expr, bindings).map(|_| ())
}

/// Like `typecheck_program_with_bindings`, but returns the displayed
/// type of the program's final expression — the REPL prints it after
/// each evaluated value.
pub fn typecheck_program_with_bindings_typed<I, S>(
    expr: &Expr,
    bindings: I,
) -> Result<String, Diagnostic>
where
    I: IntoIterator<Item = (S, KnownType)>,
    S: Into<String>,
{
    let mut checker = TypeChecker::default();
    checker.push_scope();
    checker.install_builtins();
    checker.record_schemas.extend(resolve_user_record_schemas());
    checker.enum_schemas.extend(resolve_user_enum_schemas());
    checker.typeclasses.extend(resolve_user_typeclass_infos());
    checker.instances.extend(resolve_user_instance_types());
    for (name, stored) in resolve_user_binding_types() {
        let adopted = checker.adopt_stored_type(&stored);
        checker.declare(
            name,
            false,
            adopted.ty,
            adopted.generalized_vars,
            adopted.constraints,
        );
    }
    for (binding, ty) in bindings {
        let binding = binding.into();
        if checker.lookup(&binding).is_none() {
            checker.declare(binding, false, from_known_type(ty), Vec::new(), Vec::new());
        }
    }
    let result = checker
        .infer_program(expr)
        .map(|ty| display_type(&checker.resolve(&ty)));
    if result.is_ok() {
        export_user_record_schemas(checker.user_record_schemas());
        export_user_enum_schemas(checker.user_enum_schemas());
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

pub fn clear_user_enum_schemas() {
    USER_ENUM_SCHEMAS.with(|schemas| schemas.borrow_mut().clear());
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

    /// Reassign every `Type::Var` / `Type::RowVar` id inside a stored
    /// type to fresh ids drawn from this checker's `next_var` pool.
    /// Without this step a `StoredType` imported from a previously
    /// typechecked module can collide with an unrelated fresh var
    /// allocated locally — surfacing as bogus "X not compatible with Y"
    /// errors when two modules happen to land on the same id space.
    /// Both generalized and free vars are renumbered, so leakage of
    /// non-generalized free vars (e.g. recursive definitions whose own
    /// placeholder was in scope during generalization) is also neutralised.
    fn adopt_stored_type(&mut self, stored: &StoredType) -> StoredType {
        let mut type_map: HashMap<u32, u32> = HashMap::new();
        let mut row_map: HashMap<u32, u32> = HashMap::new();
        collect_var_ids(&stored.ty, &mut type_map, &mut row_map);
        for v in &stored.generalized_vars {
            match v {
                GenericVar::Type(id) => {
                    type_map.entry(*id).or_insert(0);
                }
                GenericVar::Row(id) => {
                    row_map.entry(*id).or_insert(0);
                }
            }
        }
        for c in &stored.constraints {
            for arg in &c.arguments {
                collect_var_ids(arg, &mut type_map, &mut row_map);
            }
        }
        for value in type_map.values_mut() {
            *value = self.next_var;
            self.next_var += 1;
        }
        for value in row_map.values_mut() {
            *value = self.next_var;
            self.next_var += 1;
        }
        StoredType {
            ty: rewrite_var_ids(&stored.ty, &type_map, &row_map),
            generalized_vars: stored
                .generalized_vars
                .iter()
                .map(|v| match v {
                    GenericVar::Type(id) => GenericVar::Type(*type_map.get(id).unwrap_or(id)),
                    GenericVar::Row(id) => GenericVar::Row(*row_map.get(id).unwrap_or(id)),
                })
                .collect(),
            constraints: stored
                .constraints
                .iter()
                .map(|c| Constraint {
                    class_name: c.class_name.clone(),
                    arguments: c
                        .arguments
                        .iter()
                        .map(|t| rewrite_var_ids(t, &type_map, &row_map))
                        .collect(),
                })
                .collect(),
        }
    }

    fn lookup(&self, name: &str) -> Option<&Binding> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    /// Drop the innermost binding for `name` (used to retire a def
    /// predeclaration before generalizing its final signature).
    fn remove_binding(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.remove(name).is_some() {
                return;
            }
        }
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

    fn user_enum_schemas(&self) -> HashMap<String, EnumSchema> {
        self.enum_schemas.clone()
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
            klassic_syntax::Pattern::Constructor { name, args, span } => {
                // A known variant pins the scrutinee to its enum (with
                // fresh type arguments) and types each sub-pattern at
                // the instantiated field type; unknown constructors
                // keep the legacy Dynamic bindings.
                //
                // Resolve the constructor against the scrutinee's own enum
                // first when it has a variant of this name. A user enum may
                // reuse a prelude constructor name (`Ok`/`Err`/`Some`/`None`),
                // and a blind `enum_variant_schema` lookup returns whichever
                // enum the schema map happens to iterate first — a
                // per-process-random order that made the same `case Ok(...)`
                // on a user `Outcome` compile or fail nondeterministically.
                let scrutinee_enum = match self.resolve(scrutinee_type) {
                    Type::Enum(enum_name, _) => Some(enum_name),
                    _ => None,
                };
                let resolved_schema = scrutinee_enum
                    .as_deref()
                    .and_then(|enum_name| self.enum_variant_schema_in(enum_name, name))
                    .or_else(|| self.enum_variant_schema(name));
                let Some((enum_name, type_params, field_types)) = resolved_schema else {
                    // An unknown constructor matched against a concrete enum
                    // scrutinee is a mistake (e.g. a typo). Only fall back to
                    // Dynamic bindings when the scrutinee's enum isn't known.
                    if let Type::Enum(scrutinee_enum, _) = self.resolve(scrutinee_type) {
                        return Err(type_error(
                            *span,
                            format!("`{name}` is not a constructor of `{scrutinee_enum}`"),
                        ));
                    }
                    for arg in args {
                        self.declare_pattern_bindings(arg, &Type::Dynamic)?;
                    }
                    return Ok(());
                };
                if args.len() != field_types.len() {
                    return Err(type_error(
                        *span,
                        format!(
                            "variant `{name}` expects {} {} but got {}",
                            field_types.len(),
                            if field_types.len() == 1 {
                                "field"
                            } else {
                                "fields"
                            },
                            args.len()
                        ),
                    ));
                }
                let substitutions: HashMap<String, Type> = type_params
                    .iter()
                    .map(|param| (param.clone(), self.fresh_var()))
                    .collect();
                let instantiated = Type::Enum(
                    enum_name,
                    type_params
                        .iter()
                        .map(|param| substitutions[param].clone())
                        .collect(),
                );
                let _ = self.unify(scrutinee_type.clone(), instantiated, *span)?;
                for (arg, field) in args.iter().zip(field_types.iter()) {
                    let field_type = substitute_generics(field, &substitutions);
                    let field_type = self.resolve(&field_type);
                    self.declare_pattern_bindings(arg, &field_type)?;
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
            Type::Applied(head, args) => {
                let head = self.resolve(head);
                let args: Vec<Type> = args.iter().map(|arg| self.resolve(arg)).collect();
                // Annotations parse enum names as `Named` / applied
                // `Named`; normalize to the nominal enum type here so
                // every consumer sees one representation. The head may
                // already have normalized to a bare (zero-argument)
                // enum on its own resolve — adopt the application's
                // arguments in that case.
                match &head {
                    Type::Named(name) if self.enum_schemas.contains_key(name) => {
                        return Type::Enum(name.clone(), args);
                    }
                    Type::Enum(name, head_args) if head_args.is_empty() => {
                        return Type::Enum(name.clone(), args);
                    }
                    Type::Named(name) if self.record_schemas.contains_key(name) => {
                        return Type::Record(name.clone(), args);
                    }
                    Type::Record(name, head_args) if head_args.is_empty() => {
                        return Type::Record(name.clone(), args);
                    }
                    _ => {}
                }
                Type::Applied(Box::new(head), args)
            }
            Type::Named(name) if self.enum_schemas.contains_key(name) => {
                Type::Enum(name.clone(), Vec::new())
            }
            // Annotations parse a nominal record name as `Named`; normalize
            // it to the nominal record type so a `p: Point` annotation
            // unifies with a `#Point(...)` constructor (which is already a
            // `Type::Record`). Without this the annotation stayed `Named` and
            // failed to unify (`Point is not compatible with #Point`).
            Type::Named(name) if self.record_schemas.contains_key(name) => {
                Type::Record(name.clone(), Vec::new())
            }
            Type::Record(name, args) => Type::Record(
                name.clone(),
                args.iter().map(|arg| self.resolve(arg)).collect(),
            ),
            Type::Enum(name, args) => Type::Enum(
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
            (Type::Enum(left_name, left_args), Type::Enum(right_name, right_args))
                if left_name == right_name =>
            {
                self.unify_enum_types(left_name, left_args, right_args, span)
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
            (Type::Enum(left_name, left_args), Type::Enum(right_name, right_args))
                if left_name == right_name =>
            {
                self.unify_enum_types(left_name, left_args, right_args, span)
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
            // Width subtyping (assignability only): once the expected row has
            // no more required fields, the actual record may still carry extra
            // fields (or an open tail). A wider record is assignable to the
            // narrower expected type, so the extras are ignored — the
            // documented row-polymorphic behavior where a function "looks for
            // the fields it mentions and ignores the rest". This is sound only
            // for the directional assignability check, not for `unify`.
            (Type::RowEmpty, actual) if is_row_type(&actual) => Ok(actual),
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

    /// Mirror of `unify_record_types` for nominal enums: an empty
    /// argument list (a bare `Option` annotation) adopts the other
    /// side's arguments; otherwise arguments unify pairwise.
    fn unify_enum_types(
        &mut self,
        name: String,
        left_args: Vec<Type>,
        right_args: Vec<Type>,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        if left_args.is_empty() {
            return Ok(Type::Enum(name, right_args));
        }
        if right_args.is_empty() {
            return Ok(Type::Enum(name, left_args));
        }
        if left_args.len() != right_args.len() {
            return Err(type_error(
                span,
                format!("enum `{name}` used with mismatched type argument counts"),
            ));
        }
        let mut args = Vec::with_capacity(left_args.len());
        for (left, right) in left_args.into_iter().zip(right_args) {
            args.push(self.unify(left, right, span)?);
        }
        Ok(Type::Enum(name, args))
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
            Expr::Block { expressions, span } => {
                self.predeclare_proofs(expressions);
                self.predeclare_defs(expressions);
                let mut last = Type::Unit;
                for expression in expressions {
                    last = self.infer_expr(expression)?;
                }
                self.settle_inferred_constraints(*span)?;
                Ok(last)
            }
            other => {
                let ty = self.infer_expr(other)?;
                self.settle_inferred_constraints(other.span())?;
                Ok(ty)
            }
        }
    }

    /// Resolve the operator constraints that never reached a `def`
    /// signature (e.g. a top-level lambda, or arithmetic on a variable
    /// that was only ever used by `+`/`-`/...). A constraint whose
    /// variable became concrete must satisfy its class; an unresolved
    /// variable is ambiguous and defaults to `Int`.
    fn settle_inferred_constraints(&mut self, span: Span) -> Result<(), Diagnostic> {
        let pending = std::mem::take(&mut self.inferred_constraints);
        for constraint in pending {
            let resolved = self.resolve_constraint(&constraint);
            let vars = free_vars_in_constraints(std::slice::from_ref(&resolved));
            if vars.is_empty() {
                self.ensure_constraints_satisfied(std::slice::from_ref(&resolved), span)?;
            } else {
                for var in vars {
                    if let GenericVar::Type(id) = var {
                        self.bind_var(GenericVar::Type(id), Type::Int);
                    }
                }
            }
        }
        Ok(())
    }

    /// Fold operator constraints inferred since `since` into a generalized
    /// binding: constraints over the signature's own quantifiable variables
    /// join `constraints`, concrete ones are checked immediately, and a
    /// constraint over a variable that is not quantifiable here is pushed
    /// back to settle in an enclosing scope. Returns the (re-resolved)
    /// signature, its generalized variables, and the final constraint set.
    fn generalize_with_operator_constraints(
        &mut self,
        since: usize,
        final_type: &Type,
        mut constraints: Vec<Constraint>,
        span: Span,
    ) -> Result<(Type, Vec<GenericVar>, Vec<Constraint>), Diagnostic> {
        let inferred = self.inferred_constraints.split_off(since);
        let candidate = self.generalize_signature(final_type, &constraints);
        for constraint in inferred {
            let resolved = self.resolve_constraint(&constraint);
            let vars = free_vars_in_constraints(std::slice::from_ref(&resolved));
            if vars.is_empty() {
                self.ensure_constraints_satisfied(std::slice::from_ref(&resolved), span)?;
            } else if vars.iter().all(|var| candidate.contains(var)) {
                if !constraints.contains(&resolved) {
                    constraints.push(resolved);
                }
            } else {
                self.inferred_constraints.push(resolved);
            }
        }
        let resolved_final = self.resolve(final_type);
        let generalized = self.generalize_signature(&resolved_final, &constraints);
        Ok((resolved_final, generalized, constraints))
    }

    /// Declare every `def` in the block before checking any statement,
    /// so functions can forward-reference each other (mutual
    /// recursion). The tentative parameter/return types are remembered
    /// and reused by the `DefDecl` inference, which keeps a forward
    /// call's constraints connected to the definition's final type.
    /// Defs with type parameters or constraints keep sequential
    /// scoping for now — their annotation-shared variables would
    /// disconnect from a separately-built constraint set.
    fn predeclare_defs(&mut self, expressions: &[Expr]) {
        for expression in expressions {
            let Expr::DefDecl {
                name,
                type_params,
                constraints,
                params,
                param_annotations,
                return_annotation,
                ..
            } = expression
            else {
                continue;
            };
            if !type_params.is_empty() || !constraints.is_empty() || self.lookup(name).is_some() {
                continue;
            }
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
            let function_type = Type::Function(param_types.clone(), Box::new(return_type.clone()));
            self.declare(name.clone(), false, function_type, Vec::new(), Vec::new());
            self.predeclared_defs
                .insert(name.clone(), (param_types, return_type));
        }
    }

    // A flat, left-leaning operator chain (`1+1+1+...+1`) makes `infer_expr`
    // recurse one native Rust frame per `+`, since `Expr::Binary`'s case
    // infers `lhs` before it can unify the whole node — there is no
    // trampoline here, same as `klassic-rewrite::rewrite` and
    // `klassic-eval::eval_expr`. A chain of a few thousand terms is
    // ordinary user code and must type-check, not be rejected, but plain
    // recursion overflowed a 2 MiB debug test-thread stack at roughly 50-60
    // terms (`infer_expr`'s frame is large: HM inference threads a
    // `RefCell`-backed substitution table, constraint list, and scope stack
    // through every call, plus this match arm alone unifies both operand
    // types and builds diagnostics inline). `stacker::maybe_grow` adds a
    // fresh stack segment on demand, mirroring the guard already used for
    // `eval_expr` (added for #435) and `rewrite` (added alongside this
    // fix). The knobs reuse `klassic-eval`'s: 512 KiB red zone comfortably
    // exceeds a single `infer_expr` frame, and 8 MiB growth keeps segment
    // allocations infrequent for very long chains (100k+ terms).
    const STACK_RED_ZONE: usize = 512 * 1024;
    const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;

    fn infer_expr(&mut self, expr: &Expr) -> Result<Type, Diagnostic> {
        stacker::maybe_grow(Self::STACK_RED_ZONE, Self::STACK_GROW_SIZE, || {
            self.infer_expr_inner(expr)
        })
    }

    fn infer_expr_inner(&mut self, expr: &Expr) -> Result<Type, Diagnostic> {
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
            Expr::StringInterpolation { parts, .. } => {
                // Type-check every hole so a type error inside `#{ ... }` is
                // reported; any value is displayable, so the whole literal
                // is a `String`.
                for part in parts {
                    if let StringPart::Interpolation(hole) = part {
                        self.infer_expr(hole)?;
                    }
                }
                Ok(Type::String)
            }
            Expr::Null { .. } => Ok(Type::Null),
            Expr::Unit { .. } => Ok(Type::Unit),
            Expr::Identifier { name, span } => {
                if let Some(binding) = self.lookup(name).cloned() {
                    if !binding.constraints.is_empty() {
                        // Referencing a constrained binding indirectly (a val,
                        // a higher-order argument, ...) must carry its
                        // constraints, or a bad instantiation would type-check
                        // and crash at run time. Instantiate type + constraints
                        // with shared fresh variables and record them so the
                        // enclosing generalization (or the program-end settle)
                        // checks them — e.g. `val f: (Boolean) => String =
                        // display` (where `display` needs `Show`) now reports
                        // the missing `Show<Boolean>` instance instead of
                        // dropping the constraint and failing at runtime. A
                        // still-free constraint variable is generalized rather
                        // than rejected, so storing a polymorphic constrained
                        // function in a `val` and calling it later still works.
                        let (ty, constraints) = self.instantiate_binding_signature(&binding);
                        for constraint in constraints {
                            if !self.inferred_constraints.contains(&constraint) {
                                self.inferred_constraints.push(constraint);
                            }
                        }
                        Ok(ty)
                    } else {
                        Ok(self.instantiate(&binding))
                    }
                } else if let Some(stored) = resolve_module_selector_type(name) {
                    let adopted = self.adopt_stored_type(&stored);
                    Ok(self.instantiate_stored_type(&adopted))
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
                methods,
                span,
                ..
            } => {
                self.register_instance(class_name, for_type_annotation, constraints, *span)?;
                // Type-check each instance method against the class. Two checks:
                // (1) its body must match its declared return type, and (2) its
                // declared signature must match the class method signature
                // instantiated at the instance type — so an instance whose
                // method returns the wrong type
                // (`def compute(x: Boolean): Int = "not an int"`) or takes the
                // wrong type (`instance Compute<String> { def compute(x: Int) }`)
                // is rejected at compile time instead of crashing at runtime.
                // The method name itself is deliberately NOT declared in the
                // scratch scope: a class method called inside the body
                // (`show(p.name)`) must dispatch through the polymorphic class
                // method, not recurse into this instance's monomorphic one.
                let class_info = self.typeclasses.get(class_name).cloned();
                for method in methods {
                    let Expr::DefDecl {
                        name: method_name,
                        params,
                        param_annotations,
                        return_annotation,
                        body,
                        span: method_span,
                        ..
                    } = method
                    else {
                        continue;
                    };
                    self.push_scope();
                    let result = (|| {
                        let mut named = HashMap::new();
                        let mut param_types = Vec::with_capacity(params.len());
                        for (index, param) in params.iter().enumerate() {
                            let ty = param_annotations
                                .get(index)
                                .and_then(|annotation| annotation.as_ref())
                                .map(|annotation| {
                                    self.parse_annotation_with_named_vars(annotation, &mut named)
                                })
                                .unwrap_or_else(|| self.fresh_var());
                            self.declare(param.clone(), false, ty.clone(), Vec::new(), Vec::new());
                            param_types.push(ty);
                        }
                        let return_type = return_annotation
                            .as_ref()
                            .map(|annotation| {
                                self.parse_annotation_with_named_vars(annotation, &mut named)
                            })
                            .unwrap_or_else(|| self.fresh_var());

                        // (2) signature vs class method instantiated at the
                        // instance type. Single-parameter classes only; the
                        // shared `named` map keeps a generic instance's type
                        // variable (`Show<List<'a>>`) consistent between the
                        // instance type and the method signature.
                        if let Some(info) = &class_info
                            && info.type_params.len() == 1
                            && let Some(class_method) =
                                info.methods.iter().find(|m| &m.name == method_name)
                        {
                            let instance_type = self
                                .parse_annotation_with_named_vars(for_type_annotation, &mut named);
                            let instance_type = self.normalize_constraint_type(instance_type);
                            let substitutions: HashMap<String, Type> =
                                std::iter::once((info.type_params[0].clone(), instance_type))
                                    .collect();
                            let expected = substitute_generics(&class_method.ty, &substitutions);
                            // Only compare when the class declares the member
                            // as a function; a non-function member type is a
                            // loose placeholder, not a signature to match.
                            if matches!(self.resolve(&expected), Type::Function(_, _)) {
                                let actual = Type::Function(
                                    param_types.clone(),
                                    Box::new(return_type.clone()),
                                );
                                self.unify(expected, actual, *method_span)?;
                            }
                        }

                        // (1) body vs declared return type.
                        let body_type = self.infer_expr(body)?;
                        self.enforce_assignable(return_type, body_type, body.span())?;
                        Ok(())
                    })();
                    self.pop_scope();
                    result?;
                }
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
                self.register_enum_declaration(expr);
                Ok(Type::Unit)
            }
            Expr::Match {
                scrutinee,
                arms,
                span,
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
                    // Point a cross-arm type mismatch at the conflicting body
                    // expression, not at the start of the arm's pattern.
                    arm_type = self.unify(arm_type, body_type, arm.body.span())?;
                }
                self.check_match_coverage(&scrutinee_type, arms, *span)?;
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
                let constraints_before = self.inferred_constraints.len();
                let value_type = self.infer_expr(value)?;
                let final_type = if let Some(annotation) = annotation {
                    let mut named = HashMap::new();
                    let declared = self.parse_annotation_with_named_vars(annotation, &mut named);
                    // Point a declared-vs-value mismatch at the value, not at
                    // the start of the `val`/`mutable` keyword.
                    self.enforce_assignable(declared, value_type, value.span())?
                } else {
                    value_type
                };
                let final_type = self.resolve(&final_type);
                let (final_type, generalized_vars, constraints) = if *mutable {
                    // Mutable bindings don't generalize; any operator
                    // constraints they introduced settle at program end.
                    (final_type, Vec::new(), Vec::new())
                } else {
                    self.generalize_with_operator_constraints(
                        constraints_before,
                        &final_type,
                        Vec::new(),
                        *span,
                    )?
                };
                self.declare(
                    name.clone(),
                    *mutable,
                    final_type,
                    generalized_vars,
                    constraints,
                );
                Ok(Type::Unit)
            }
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
                let mut named = HashMap::new();
                // Seed the annotation parser with the explicit type
                // parameters so a bare `<a>` name is a type variable, like
                // enums/records/extensions. Without this only the `'a` form
                // became a variable and `def id<a>(x: a): a` treated `a` as a
                // concrete type, so `id(5)` failed to unify.
                for type_param in type_params {
                    named
                        .entry(type_param.clone())
                        .or_insert_with(|| self.fresh_var());
                }
                // Reuse the block-level predeclaration's variables when
                // one exists, so forward calls already checked against
                // them constrain this definition.
                let predeclared = self.predeclared_defs.remove(name);
                let was_predeclared = predeclared.is_some();
                let (mut param_types, return_type) = if let Some((params, ret)) = predeclared {
                    (params, ret)
                } else {
                    let param_types = param_annotations
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
                    let return_type = return_annotation
                        .as_ref()
                        .map(|annotation| {
                            self.parse_annotation_with_named_vars(annotation, &mut named)
                        })
                        .unwrap_or_else(|| self.fresh_var());
                    (param_types, return_type)
                };
                if param_types.len() < params.len() {
                    param_types.resize_with(params.len(), || self.fresh_var());
                }
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
                let constraints_before = self.inferred_constraints.len();
                let body_type = self.infer_expr(body)?;
                self.pop_scope();

                // Point a return-type mismatch at the body expression, not at
                // the start of the `def` keyword.
                let resolved_return =
                    self.enforce_assignable(return_type, body_type, body.span())?;
                let resolved_params = param_types
                    .into_iter()
                    .map(|param| self.resolve(&param))
                    .collect();
                let final_type =
                    Type::Function(resolved_params, Box::new(self.resolve(&resolved_return)));
                // The block-level predeclaration left a monomorphic
                // binding whose variables would otherwise count as
                // environment-bound and block generalization — drop it
                // before quantifying; the final declaration replaces it.
                if was_predeclared {
                    self.remove_binding(name);
                }
                // Fold operator constraints inferred from this body into the
                // generalized signature (`forall a: Plus. (a, a) -> a`).
                let (final_type, generalized_vars, resolved_constraints) = self
                    .generalize_with_operator_constraints(
                        constraints_before,
                        &final_type,
                        resolved_constraints,
                        *span,
                    )?;
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
                        match self.resolve(&inner) {
                            Type::Var(id) => {
                                let constraint = Constraint {
                                    class_name: "Num".to_string(),
                                    arguments: vec![Type::Var(id)],
                                };
                                if !self.inferred_constraints.contains(&constraint) {
                                    self.inferred_constraints.push(constraint);
                                }
                                Ok(Type::Var(id))
                            }
                            other if other.is_numeric() || other.is_dynamic_like() => Ok(other),
                            _ => Err(type_error(*span, "unary numeric operator expects a number")),
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
                        match self.resolve(&unified) {
                            // Operands stay an unresolved variable: instead of
                            // eagerly defaulting to Int, record an operator
                            // constraint so the enclosing `def` generalizes over
                            // a `Num` / `Plus` type. `+` also accepts strings.
                            Type::Var(id) => {
                                let class_name = if matches!(op, BinaryOp::Add) {
                                    "Plus"
                                } else {
                                    "Num"
                                };
                                let constraint = Constraint {
                                    class_name: class_name.to_string(),
                                    arguments: vec![Type::Var(id)],
                                };
                                if !self.inferred_constraints.contains(&constraint) {
                                    self.inferred_constraints.push(constraint);
                                }
                                Ok(Type::Var(id))
                            }
                            other if other.is_numeric() || other.is_dynamic_like() => Ok(other),
                            other => Err(type_error(
                                *span,
                                format!(
                                    "arithmetic operator requires a numeric type, but got {}",
                                    display_type(&other)
                                ),
                            )),
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
                let noun = self.call_target_noun(callee);
                if let Expr::Identifier { name, .. } = callee.as_ref()
                    && let Some(binding) = self.lookup(name).cloned()
                    && !binding.constraints.is_empty()
                {
                    let (callee_type, constraints) = self.instantiate_binding_signature(&binding);
                    let result =
                        self.infer_constrained_call(callee_type, arguments, *span, noun)?;
                    self.ensure_constraints_satisfied(&constraints, *span)?;
                    return Ok(result);
                }
                let callee_type = self.infer_expr(callee)?;
                self.infer_call(callee_type, arguments, *span, noun)
            }
            Expr::FieldAccess {
                target,
                field,
                span,
            } => {
                // `M.member` where `M` is an aliased module import: the
                // import declared `M#member` bindings, so dot access on
                // an otherwise-unbound identifier resolves through them
                // (parity with the native backend's rewrite).
                if let Expr::Identifier { name, .. } = target.as_ref()
                    && self.lookup(name).is_none()
                    && let Some(binding) = self.lookup(&format!("{name}#{field}")).cloned()
                {
                    return Ok(self.instantiate(&binding));
                }
                let target_type = self.infer_expr(target)?;
                if let Some(method_type) = self.builtin_value_method_type(&target_type, field) {
                    return Ok(method_type);
                }
                if let Some(method_type) =
                    self.user_extension_method_type(&target_type, field, *span)
                {
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
                        format!("no method or field `{}` on {}", field, display_type(&other)),
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
                self.predeclare_defs(expressions);
                let mut last = Type::Unit;
                for expression in expressions {
                    last = self.infer_expr(expression)?;
                }
                self.pop_scope();
                Ok(last)
            }
        }
    }

    /// Whether a call target is an enum constructor, so arity diagnostics
    /// can say "constructor" instead of the generic "function".
    fn call_target_noun(&self, callee: &Expr) -> &'static str {
        if let Expr::Identifier { name, .. } = callee
            && self.enum_variant_schema(name).is_some()
        {
            "constructor"
        } else {
            "function"
        }
    }

    fn infer_call(
        &mut self,
        callee_type: Type,
        arguments: &[Expr],
        span: Span,
        noun: &str,
    ) -> Result<Type, Diagnostic> {
        match self.resolve(&callee_type) {
            Type::Function(params, result) => {
                if arguments.len() < params.len() {
                    return Err(type_error(
                        span,
                        format!(
                            "{noun} expects {} {} but got {}",
                            params.len(),
                            if params.len() == 1 {
                                "argument"
                            } else {
                                "arguments"
                            },
                            arguments.len()
                        ),
                    ));
                }
                let arity = params.len();
                // Check non-lambda arguments before lambda arguments: a
                // lambda whose body dispatches on its parameter (e.g.
                // `(lst) => lst.size()`) would otherwise commit that
                // parameter's still-free type variable to a structural-record
                // shape before a sibling argument pins it to the real type, so
                // `hof((lst) => lst.size(), [1,2,3])` would spuriously fail.
                let pairs: Vec<(Type, &Expr)> = params.into_iter().zip(arguments.iter()).collect();
                for (expected, argument) in &pairs {
                    if !matches!(argument, Expr::Lambda { .. }) {
                        self.check_call_argument(expected.clone(), argument)?;
                    }
                }
                for (expected, argument) in &pairs {
                    if matches!(argument, Expr::Lambda { .. }) {
                        self.check_call_argument(expected.clone(), argument)?;
                    }
                }
                let result = self.resolve(&result);
                if arguments.len() > arity {
                    // Over-application: a curried function applied to more
                    // arguments than its first arity. Feed the remaining
                    // arguments to the returned function, matching the runtime
                    // (which curries via `apply_callable`), so e.g.
                    // `map(xs, f)` type-checks like `map(xs)(f)`. Only when the
                    // result is itself a function — otherwise this is a genuine
                    // too-many-arguments error (e.g. a constructor or a
                    // scalar-returning function), reported as such.
                    if matches!(result, Type::Function(_, _)) {
                        return self.infer_call(result, &arguments[arity..], span, noun);
                    }
                    return Err(type_error(
                        span,
                        format!(
                            "{noun} expects {} {} but got {}",
                            arity,
                            if arity == 1 { "argument" } else { "arguments" },
                            arguments.len()
                        ),
                    ));
                }
                Ok(result)
            }
            Type::Var(id) => {
                // The callee is an unresolved type variable — typically a
                // function-typed parameter like `f` in `def apply(f, x) =
                // f(x)`. Unify it with a function type built from the
                // argument types so the parameter's signature is recovered
                // and the arguments are type-checked, instead of erasing the
                // whole call to `Dynamic` (which silently accepts a
                // wrongly-typed argument and crashes at run time).
                let arg_types = arguments
                    .iter()
                    .map(|argument| self.infer_expr(argument))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.fresh_var();
                let function = Type::Function(arg_types, Box::new(result.clone()));
                self.unify(Type::Var(id), function, span)?;
                Ok(self.resolve(&result))
            }
            Type::Dynamic | Type::Null => Ok(Type::Dynamic),
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
        noun: &str,
    ) -> Result<Type, Diagnostic> {
        match self.resolve(&callee_type) {
            Type::Function(params, result) => {
                if arguments.len() < params.len() {
                    return Err(type_error(
                        span,
                        format!(
                            "{noun} expects {} {} but got {}",
                            params.len(),
                            if params.len() == 1 {
                                "argument"
                            } else {
                                "arguments"
                            },
                            arguments.len()
                        ),
                    ));
                }
                let arity = params.len();
                // Check non-lambda arguments before lambda arguments: a
                // lambda whose body dispatches on its parameter (e.g.
                // `(lst) => lst.size()`) would otherwise commit that
                // parameter's still-free type variable to a structural-record
                // shape before a sibling argument pins it to the real type, so
                // `hof((lst) => lst.size(), [1,2,3])` would spuriously fail.
                let pairs: Vec<(Type, &Expr)> = params.into_iter().zip(arguments.iter()).collect();
                for (expected, argument) in &pairs {
                    if !matches!(argument, Expr::Lambda { .. }) {
                        self.check_call_argument(expected.clone(), argument)?;
                    }
                }
                for (expected, argument) in &pairs {
                    if matches!(argument, Expr::Lambda { .. }) {
                        self.check_call_argument(expected.clone(), argument)?;
                    }
                }
                let result = self.resolve(&result);
                if arguments.len() > arity {
                    // Over-application: a curried function applied to more
                    // arguments than its first arity. Feed the remaining
                    // arguments to the returned function, matching the runtime
                    // (which curries via `apply_callable`), so e.g.
                    // `map(xs, f)` type-checks like `map(xs)(f)`. Only when the
                    // result is itself a function — otherwise this is a genuine
                    // too-many-arguments error (e.g. a constructor or a
                    // scalar-returning function), reported as such.
                    if matches!(result, Type::Function(_, _)) {
                        return self.infer_call(result, &arguments[arity..], span, noun);
                    }
                    return Err(type_error(
                        span,
                        format!(
                            "{noun} expects {} {} but got {}",
                            arity,
                            if arity == 1 { "argument" } else { "arguments" },
                            arguments.len()
                        ),
                    ));
                }
                Ok(result)
            }
            Type::Var(id) => {
                // The callee is an unresolved type variable — typically a
                // function-typed parameter like `f` in `def apply(f, x) =
                // f(x)`. Unify it with a function type built from the
                // argument types so the parameter's signature is recovered
                // and the arguments are type-checked, instead of erasing the
                // whole call to `Dynamic` (which silently accepts a
                // wrongly-typed argument and crashes at run time).
                let arg_types = arguments
                    .iter()
                    .map(|argument| self.infer_expr(argument))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.fresh_var();
                let function = Type::Function(arg_types, Box::new(result.clone()));
                self.unify(Type::Var(id), function, span)?;
                Ok(self.resolve(&result))
            }
            Type::Dynamic | Type::Null => Ok(Type::Dynamic),
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
            "__native_atomic_self_test".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__native_thread_spawn_test".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__native_thread_safe_alloc_test".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__native_thread_safe_shadow_test".to_string(),
            false,
            Type::Function(vec![Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__native_thread_safe_collect_test".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__native_stack_floor_is_disabled".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__native_zgc_relocation_test".to_string(),
            false,
            Type::Function(vec![], Box::new(Type::Int)),
        );
        self.declare_poly(
            "__native_zgc_relocate_many_test".to_string(),
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
            "String#parseInt".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Int)),
        );
        self.declare_poly(
            "String#parseDouble".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Double)),
        );
        self.declare_poly(
            "String#parseIntOr".to_string(),
            false,
            Type::Function(vec![Type::String, Type::Int], Box::new(Type::Int)),
        );
        self.declare_poly(
            "String#parseDoubleOr".to_string(),
            false,
            Type::Function(vec![Type::String, Type::Double], Box::new(Type::Double)),
        );
        self.declare_poly(
            "String#isInt".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Bool)),
        );
        self.declare_poly(
            "String#isDouble".to_string(),
            false,
            Type::Function(vec![Type::String], Box::new(Type::Bool)),
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
            "round".to_string(),
            false,
            Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
        );
        for name in [
            "sin", "cos", "tan", "asin", "acos", "atan", "exp", "log", "log10", "log2",
        ] {
            self.declare_poly(
                name.to_string(),
                false,
                Type::Function(vec![Type::Dynamic], Box::new(Type::Double)),
            );
        }
        for name in ["pow", "atan2"] {
            self.declare_poly(
                name.to_string(),
                false,
                Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Double)),
            );
        }
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
        self.declare_poly(
            "padStart".to_string(),
            false,
            Type::Function(
                vec![Type::String, Type::Int, Type::String],
                Box::new(Type::String),
            ),
        );
        self.declare_poly(
            "padEnd".to_string(),
            false,
            Type::Function(
                vec![Type::String, Type::Int, Type::String],
                Box::new(Type::String),
            ),
        );
        self.declare_poly(
            "format".to_string(),
            false,
            Type::Function(
                vec![Type::String, Type::List(Box::new(Type::Dynamic))],
                Box::new(Type::String),
            ),
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
                    "getOrElse",
                    Type::Function(
                        vec![Type::Dynamic, Type::Dynamic, Type::Dynamic],
                        Box::new(Type::Dynamic),
                    ),
                ),
                // Element-typed over fresh generalized variables (declared
                // `declare_poly`, so each use instantiates fresh): the key /
                // value type flows out instead of `Dynamic`.
                (
                    "keys",
                    Type::Function(
                        vec![Type::Map(Box::new(Type::Var(0)), Box::new(Type::Var(1)))],
                        Box::new(Type::List(Box::new(Type::Var(0)))),
                    ),
                ),
                (
                    "values",
                    Type::Function(
                        vec![Type::Map(Box::new(Type::Var(0)), Box::new(Type::Var(1)))],
                        Box::new(Type::List(Box::new(Type::Var(1)))),
                    ),
                ),
                (
                    "isEmpty",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Bool)),
                ),
                (
                    "size",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
                ),
                (
                    "put",
                    Type::Function(
                        vec![Type::Dynamic, Type::Dynamic, Type::Dynamic],
                        Box::new(Type::Dynamic),
                    ),
                ),
                (
                    "remove",
                    Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Dynamic)),
                ),
                (
                    "fromPairs",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
                ),
                ("empty", Type::Function(vec![], Box::new(Type::Dynamic))),
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
                (
                    "add",
                    Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Dynamic)),
                ),
                (
                    "remove",
                    Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Dynamic)),
                ),
                (
                    "fromList",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
                ),
                (
                    "toList",
                    Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
                ),
                ("empty", Type::Function(vec![], Box::new(Type::Dynamic))),
                // Two sets of the same element type in, a set of that type
                // out, over a fresh generalized variable.
                (
                    "union",
                    Type::Function(
                        vec![
                            Type::Set(Box::new(Type::Var(0))),
                            Type::Set(Box::new(Type::Var(0))),
                        ],
                        Box::new(Type::Set(Box::new(Type::Var(0)))),
                    ),
                ),
                (
                    "intersect",
                    Type::Function(
                        vec![
                            Type::Set(Box::new(Type::Var(0))),
                            Type::Set(Box::new(Type::Var(0))),
                        ],
                        Box::new(Type::Set(Box::new(Type::Var(0)))),
                    ),
                ),
                (
                    "subtract",
                    Type::Function(
                        vec![
                            Type::Set(Box::new(Type::Var(0))),
                            Type::Set(Box::new(Type::Var(0))),
                        ],
                        Box::new(Type::Set(Box::new(Type::Var(0)))),
                    ),
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
            "Process" => &[
                (
                    "exit",
                    Type::Function(vec![Type::Int], Box::new(Type::Unit)),
                ),
                (
                    "run",
                    Type::Function(
                        vec![Type::String, Type::List(Box::new(Type::String))],
                        Box::new(process_run_result_type()),
                    ),
                ),
            ],
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
            let message = if path.ends_with("._") {
                let module = path.trim_end_matches("._");
                format!(
                    "wildcard imports are not supported; list the members explicitly, \
                     e.g. `import {module}.{{a, b}}`"
                )
            } else if klassic_runtime::STDLIB_MODULES
                .iter()
                .any(|module| module.path == path)
            {
                // A real stdlib module that resolved here only because it is
                // not inlined by the current backend (e.g. `std.json` natively).
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
                    let adopted = self.adopt_stored_type(stored);
                    self.declare_stored(format!("{alias}#{name}"), false, &adopted);
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
                    let adopted = self.adopt_stored_type(&stored);
                    self.declare(
                        member.clone(),
                        false,
                        adopted.ty,
                        adopted.generalized_vars,
                        adopted.constraints,
                    );
                }
            }
            None if alias.is_none() => {
                for (name, stored) in exports {
                    if excludes.iter().any(|member| member == &name) {
                        continue;
                    }
                    let adopted = self.adopt_stored_type(&stored);
                    self.declare(
                        name,
                        false,
                        adopted.ty,
                        adopted.generalized_vars,
                        adopted.constraints,
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
                    // Mirror the enum/record path: a bare class type
                    // parameter (`typeclass Show<a> where { show: (a) => ... }`)
                    // parses to `Named("a")`, so generify it to a type
                    // variable like the `'a` form already is.
                    ty: generify_named_params(
                        parse_schema_type_annotation(&method.annotation.text, type_params),
                        type_params,
                    ),
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
                        "typeclass `{}` expects {} type {} but got {}",
                        constraint.class_name,
                        info.type_params.len(),
                        if info.type_params.len() == 1 {
                            "argument"
                        } else {
                            "arguments"
                        },
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
                // Declare the class method polymorphically over the
                // constraint's type variables with the constraint attached,
                // rather than monomorphically tied to one constraint's
                // argument. Each use then instantiates fresh and records the
                // constraint at its concrete type. This lets two constraints
                // on the same class with different type variables
                // (`def pair<Show 'a, Show 'b>`) both reach `show` without
                // unifying `'a` and `'b` — the old `declare_mono` overwrote
                // the method with the last constraint's type, forcing the two
                // variables equal and spuriously rejecting mixed-type calls.
                let ty = substitute_generics(&method.ty, &substitutions);
                let generalized = free_vars(&ty);
                self.declare(
                    method.name.clone(),
                    false,
                    ty,
                    generalized,
                    vec![constraint.clone()],
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
            Type::Enum(name, args) => Type::Enum(
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
        // Built-in operator classes are resolved structurally (they have no
        // user `instance` declarations): `Num` for `-`/`*`/`/`, `Plus` for
        // `+` (numbers and string concatenation).
        if let Some(argument) = required.arguments.first() {
            let resolved = self.resolve(argument);
            match required.class_name.as_str() {
                "Num" => return resolved.is_numeric(),
                "Plus" => return resolved.is_numeric() || matches!(resolved, Type::String),
                _ => {}
            }
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
                        "record `{name}` expects {} {} but got {}",
                        schema.fields.len(),
                        if schema.fields.len() == 1 {
                            "field"
                        } else {
                            "fields"
                        },
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
                            // Mirror the enum path: a bare type-parameter name
                            // (`record Box<a> { v: a }`) parses to `Named("a")`,
                            // so generify it to `Generic("a")` like enums do.
                            // Without this only the `'a` form became a type
                            // variable and `<a>` was treated as a concrete type,
                            // so `#Box(5)` failed to unify.
                            generify_named_params(
                                parse_schema_type_annotation(&annotation.text, type_params),
                                type_params,
                            )
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

    fn register_enum_declaration(&mut self, expr: &Expr) {
        let Expr::EnumDeclaration {
            name: enum_name,
            type_params,
            variants,
            ..
        } = expr
        else {
            return;
        };
        let mut schema_variants = Vec::new();
        for variant in variants {
            let field_types: Vec<Type> = variant
                .params
                .iter()
                .map(|(_, annotation)| {
                    generify_named_params(
                        parse_schema_type_annotation(&annotation.text, type_params),
                        type_params,
                    )
                })
                .collect();
            schema_variants.push((variant.name.clone(), field_types));
        }
        self.enum_schemas.insert(
            enum_name.clone(),
            EnumSchema {
                type_params: type_params.clone(),
                variants: schema_variants.clone(),
            },
        );
        // Each constructor is a polymorphic function into the enum
        // (`Some : forall a. (a) -> Option<a>`); a nullary variant is a
        // polymorphic value of the enum type itself. The quantified
        // positions must be fresh `Var`s — `Type::Generic` is treated
        // as dynamic-like by unification (the gradual-typing escape
        // hatch), which would let payload constraints evaporate.
        for (variant_name, field_types) in schema_variants {
            let substitutions: HashMap<String, Type> = type_params
                .iter()
                .map(|param| (param.clone(), self.fresh_var()))
                .collect();
            let enum_type = Type::Enum(
                enum_name.clone(),
                type_params
                    .iter()
                    .map(|param| substitutions[param].clone())
                    .collect(),
            );
            let ty = if field_types.is_empty() {
                enum_type
            } else {
                Type::Function(
                    field_types
                        .iter()
                        .map(|field| substitute_generics(field, &substitutions))
                        .collect(),
                    Box::new(enum_type),
                )
            };
            self.declare_poly(variant_name, false, ty);
        }
    }

    /// Exhaustiveness and reachability for a `match` over a known enum:
    /// every variant must be covered by an unguarded arm (a wildcard /
    /// variable arm covers everything; a constructor arm covers its
    /// variant), and an arm after an unguarded irrefutable one is dead.
    /// Guarded arms never count as covering — guards are undecidable.
    /// Coverage is variant-granular: refutable sub-patterns
    /// (`case Some(5)` with no other `Some` arm) still fall through to
    /// the runtime match-failure path, as do non-enum scrutinees.
    fn check_match_coverage(
        &mut self,
        scrutinee_type: &Type,
        arms: &[klassic_syntax::MatchArm],
        span: Span,
    ) -> Result<(), Diagnostic> {
        let mut closed = false;
        let mut closed_variants: HashSet<String> = HashSet::new();
        let mut closed_literals: HashSet<String> = HashSet::new();
        for arm in arms {
            if closed {
                return Err(type_error(
                    arm.span,
                    "unreachable match arm: a preceding pattern already matches every value",
                ));
            }
            // A later unguarded arm for a constructor that an earlier
            // unguarded arm already matches with irrefutable payloads is
            // dead (e.g. a second `case None` or `case Some(_)`). An arm
            // with a refutable payload such as `case Some(1)` does not
            // close the variant, so `case Some(x)` after it stays live.
            if arm.guard.is_none()
                && let klassic_syntax::Pattern::Constructor { name, args, .. } = &arm.pattern
            {
                if closed_variants.contains(name) {
                    return Err(type_error(
                        arm.span,
                        format!(
                            "unreachable match arm: `{name}` is already matched by a preceding arm"
                        ),
                    ));
                }
                if args.iter().all(pattern_is_irrefutable) {
                    closed_variants.insert(name.clone());
                }
            }
            // The same is true for a repeated literal pattern: a second
            // unguarded `case true` or `case 1` can never run because the
            // first already matches that value. A guarded literal arm does
            // not close the value, so a later arm for it stays live.
            if arm.guard.is_none()
                && let Some(key) = literal_pattern_key(&arm.pattern)
                && !closed_literals.insert(key.clone())
            {
                return Err(type_error(
                    arm.span,
                    format!("unreachable match arm: {key} is already matched by a preceding arm"),
                ));
            }
            if arm.guard.is_none() && pattern_is_irrefutable(&arm.pattern) {
                closed = true;
            }
        }
        if closed {
            return Ok(());
        }
        // A `Boolean` scrutinee has a finite domain, so an unguarded match
        // without a wildcard (which would have set `closed` above) is
        // exhaustive only when both `true` and `false` appear — mirroring
        // the enum check below and the nested `patterns_cover` rule.
        if let Type::Bool = self.resolve(scrutinee_type) {
            let unguarded: Vec<&klassic_syntax::Pattern> = arms
                .iter()
                .filter(|arm| arm.guard.is_none())
                .map(|arm| &arm.pattern)
                .collect();
            if self.patterns_cover(&unguarded, &Type::Bool) {
                return Ok(());
            }
            let has_true = unguarded
                .iter()
                .any(|p| matches!(p, klassic_syntax::Pattern::LiteralBool { value: true, .. }));
            let has_false = unguarded
                .iter()
                .any(|p| matches!(p, klassic_syntax::Pattern::LiteralBool { value: false, .. }));
            let missing = match (has_true, has_false) {
                (true, false) => "false",
                (false, true) => "true",
                _ => "true, false",
            };
            return Err(type_error(
                span,
                format!("match on `Boolean` is not exhaustive: missing {missing}"),
            ));
        }
        let Type::Enum(enum_name, enum_args) = self.resolve(scrutinee_type) else {
            return Ok(());
        };
        let Some(schema) = self.enum_schemas.get(&enum_name).cloned() else {
            return Ok(());
        };
        let patterns: Vec<&klassic_syntax::Pattern> = arms
            .iter()
            .filter(|arm| arm.guard.is_none())
            .map(|arm| &arm.pattern)
            .collect();
        let substitutions: HashMap<String, Type> = schema
            .type_params
            .iter()
            .cloned()
            .zip(enum_args.iter().cloned())
            .collect();
        // A variant is covered when it is matched and — for a single-field
        // variant — its payload sub-patterns are themselves exhaustive, so a
        // refutable payload (`case Wrap(1)`) or a nested constructor
        // (`case Full(Full(x))`) that leaves part of the variant unhandled is
        // reported as missing. Multi-field variants keep the lenient
        // presence-covers behavior (full matrix coverage is future work).
        let missing: Vec<&str> = schema
            .variants
            .iter()
            .filter(|(variant, field_types)| {
                let variant_args: Vec<&[klassic_syntax::Pattern]> = patterns
                    .iter()
                    .filter_map(|pattern| match pattern {
                        klassic_syntax::Pattern::Constructor { name, args, .. }
                            if name == variant =>
                        {
                            Some(args.as_slice())
                        }
                        _ => None,
                    })
                    .collect();
                if variant_args.is_empty() {
                    return true; // variant never matched
                }
                if field_types.is_empty() {
                    return false; // nullary variant: presence covers it
                }
                if variant_args
                    .iter()
                    .any(|args| args.iter().all(pattern_is_irrefutable))
                {
                    return false; // an irrefutable-payload arm fully covers it
                }
                if field_types.len() == 1 {
                    let field_type = substitute_generics(&field_types[0], &substitutions);
                    let field_patterns: Vec<&klassic_syntax::Pattern> =
                        variant_args.iter().map(|args| &args[0]).collect();
                    return !self.patterns_cover(&field_patterns, &field_type);
                }
                false // multi-field: leniently considered covered
            })
            .map(|(variant, _)| variant.as_str())
            .collect();
        if missing.is_empty() {
            Ok(())
        } else {
            Err(type_error(
                span,
                format!(
                    "match on `{enum_name}` is not exhaustive: missing {}",
                    missing.join(", ")
                ),
            ))
        }
    }

    /// Whether `patterns` exhaustively cover every value of `ty`, used to
    /// check a constructor variant's payload sub-patterns recursively. A
    /// wildcard or variable covers everything; a `Bool` needs both `true` and
    /// `false`; an enum needs every variant covered (recursing into
    /// single-field payloads); any other type (`Int`, `String`, ...) is only
    /// covered by a wildcard/variable.
    fn patterns_cover(&self, patterns: &[&klassic_syntax::Pattern], ty: &Type) -> bool {
        use klassic_syntax::Pattern;
        if patterns
            .iter()
            .any(|p| matches!(p, Pattern::Wildcard { .. } | Pattern::Variable { .. }))
        {
            return true;
        }
        match self.resolve(ty) {
            Type::Bool => {
                let has_true = patterns
                    .iter()
                    .any(|p| matches!(p, Pattern::LiteralBool { value: true, .. }));
                let has_false = patterns
                    .iter()
                    .any(|p| matches!(p, Pattern::LiteralBool { value: false, .. }));
                has_true && has_false
            }
            Type::Enum(enum_name, enum_args) => {
                let Some(schema) = self.enum_schemas.get(&enum_name).cloned() else {
                    return false;
                };
                let substitutions: HashMap<String, Type> = schema
                    .type_params
                    .iter()
                    .cloned()
                    .zip(enum_args.iter().cloned())
                    .collect();
                schema.variants.iter().all(|(variant, field_types)| {
                    let variant_args: Vec<&[Pattern]> = patterns
                        .iter()
                        .filter_map(|pattern| match pattern {
                            Pattern::Constructor { name, args, .. } if name == variant => {
                                Some(args.as_slice())
                            }
                            _ => None,
                        })
                        .collect();
                    if variant_args.is_empty() {
                        return false;
                    }
                    if field_types.is_empty() {
                        return true;
                    }
                    if variant_args
                        .iter()
                        .any(|args| args.iter().all(pattern_is_irrefutable))
                    {
                        return true;
                    }
                    if field_types.len() == 1 {
                        let field_type = substitute_generics(&field_types[0], &substitutions);
                        let field_patterns: Vec<&Pattern> =
                            variant_args.iter().map(|args| &args[0]).collect();
                        return self.patterns_cover(&field_patterns, &field_type);
                    }
                    true
                })
            }
            _ => false,
        }
    }

    /// Look up the enum owning `variant`, returning the enum name, its
    /// type parameters, and the variant's field types (over
    /// `Type::Generic` parameters).
    /// Resolve a constructor name against a specific enum, used to
    /// disambiguate a constructor shared by several enums in favour of the
    /// match scrutinee's own enum.
    fn enum_variant_schema_in(
        &self,
        enum_name: &str,
        variant: &str,
    ) -> Option<(String, Vec<String>, Vec<Type>)> {
        let schema = self.enum_schemas.get(enum_name)?;
        let (_, fields) = schema.variants.iter().find(|(name, _)| name == variant)?;
        Some((
            enum_name.to_string(),
            schema.type_params.clone(),
            fields.clone(),
        ))
    }

    fn enum_variant_schema(&self, variant: &str) -> Option<(String, Vec<String>, Vec<Type>)> {
        for (enum_name, schema) in &self.enum_schemas {
            if let Some((_, fields)) = schema.variants.iter().find(|(name, _)| name == variant) {
                return Some((
                    enum_name.clone(),
                    schema.type_params.clone(),
                    fields.clone(),
                ));
            }
        }
        None
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

    fn user_extension_method_type(
        &mut self,
        target: &Type,
        field: &str,
        span: Span,
    ) -> Option<Type> {
        let resolved = self.resolve(target);
        let key = dispatch_key_for_type(&resolved)?;
        let stored = resolve_user_extension_method_type(&key, field)?;
        let adopted = self.adopt_stored_type(&stored);
        let instantiated = self.instantiate_stored_type(&adopted);
        match instantiated {
            Type::Function(mut params, ret) if !params.is_empty() => {
                // Unify the receiver against the declared `this` type before
                // dropping it, so the receiver's concrete type arguments flow
                // into the method's type variables (and a concrete receiver
                // mismatch is rejected). `dispatch_key_for_type` keys only on
                // the type constructor, so without this an
                // `extension (this: List<Int>)` applied to a `List<String>`
                // kept `Int` and silently accepted ill-typed arguments, and a
                // generic `extension <a>(this: Box<a>)` left `a` free.
                let this_param = params.remove(0);
                self.unify(this_param, resolved, span).ok()?;
                Some(Type::Function(params, ret))
            }
            other => Some(other),
        }
    }

    fn builtin_value_method_type(&mut self, target: &Type, field: &str) -> Option<Type> {
        let resolved = self.resolve(target);
        match resolved {
            Type::Int
            | Type::Long
            | Type::Short
            | Type::Byte
            | Type::Float
            | Type::Double
            | Type::Bool
            | Type::Unit
            | Type::Record(_, _)
            | Type::Enum(_, _)
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
                "padStart" | "padEnd" => Some(Type::Function(
                    vec![Type::Int, Type::String],
                    Box::new(Type::String),
                )),
                "parseIntOr" => Some(Type::Function(vec![Type::Int], Box::new(Type::Int))),
                "parseDoubleOr" => Some(Type::Function(vec![Type::Double], Box::new(Type::Double))),
                "isInteger" | "isDouble" => Some(Type::Function(vec![], Box::new(Type::Bool))),
                _ => None,
            },
            Type::List(inner) => match field {
                "toString" => Some(Type::Function(vec![], Box::new(Type::String))),
                "head" => Some(Type::Function(vec![], inner.clone())),
                "tail" => Some(Type::Function(vec![], Box::new(Type::List(inner.clone())))),
                "size" => Some(Type::Function(vec![], Box::new(Type::Int))),
                "isEmpty" => Some(Type::Function(vec![], Box::new(Type::Bool))),
                // `xs.contains(x)` over a `List<a>` requires `x` to be the
                // element type `a` rather than `Dynamic`, so e.g.
                // `[1 2 3].contains("s")` on a `List<Int>` is a type error
                // instead of a silently-accepted always-false membership test.
                "contains" => Some(Type::Function(
                    vec![inner.as_ref().clone()],
                    Box::new(Type::Bool),
                )),
                "join" => Some(Type::Function(vec![Type::String], Box::new(Type::String))),
                // `xs.map(f)` over a `List<a>` is element-aware: `f` must
                // accept the element type `a` and the result is `List<b>` for
                // whatever `f` returns. This rejects, e.g., a `(b: Bool) => _`
                // mapper applied to a `List<Int>` instead of silently typing
                // the call as `Dynamic`.
                "map" => {
                    let result = self.fresh_var();
                    let mapper =
                        Type::Function(vec![inner.as_ref().clone()], Box::new(result.clone()));
                    Some(Type::Function(
                        vec![mapper],
                        Box::new(Type::List(Box::new(result))),
                    ))
                }
                // Accepts both `xs.foldLeft(init, f)` (two args at
                // once) and `xs.foldLeft(init)(f)` (curried) — the
                // latter partial-applies this type, and the runtime
                // goes through apply_callable so the partial completes.
                // `xs.foldLeft(init, f)` over a `List<a>` ties the folder to
                // the element type: `init` and the result share an
                // accumulator type `b`, and `f` is `(b, a) -> b`. This
                // rejects, e.g., a folder that treats the `Int` element as a
                // `String`, instead of typing every position as `Dynamic`.
                "foldLeft" => {
                    let acc = self.fresh_var();
                    let folder = Type::Function(
                        vec![acc.clone(), inner.as_ref().clone()],
                        Box::new(acc.clone()),
                    );
                    Some(Type::Function(vec![acc.clone(), folder], Box::new(acc)))
                }
                _ => None,
            },
            Type::Map(key, value) => match field {
                "toString" => Some(Type::Function(vec![], Box::new(Type::String))),
                // `m.containsKey(k)` / `m.containsValue(v)` / `m.get(k)` take the
                // map's declared key/value type rather than `Dynamic`, matching
                // `getOrElse`/`put`/`remove` below: a wrong-typed lookup such as
                // `%["a": 1].get(99)` is a type error instead of being silently
                // accepted. `get` keeps a `Dynamic` result because it returns
                // null for an absent key.
                "containsKey" => Some(Type::Function(vec![(*key).clone()], Box::new(Type::Bool))),
                "containsValue" => {
                    Some(Type::Function(vec![(*value).clone()], Box::new(Type::Bool)))
                }
                "get" => Some(Type::Function(
                    vec![(*key).clone()],
                    Box::new(Type::Dynamic),
                )),
                "getOrElse" => Some(Type::Function(
                    vec![(*key).clone(), (*value).clone()],
                    Box::new((*value).clone()),
                )),
                "keys" => Some(Type::Function(
                    vec![],
                    Box::new(Type::List(Box::new((*key).clone()))),
                )),
                "values" => Some(Type::Function(
                    vec![],
                    Box::new(Type::List(Box::new((*value).clone()))),
                )),
                "isEmpty" => Some(Type::Function(vec![], Box::new(Type::Bool))),
                "size" => Some(Type::Function(vec![], Box::new(Type::Int))),
                "put" => Some(Type::Function(
                    vec![(*key).clone(), (*value).clone()],
                    Box::new(Type::Map(key.clone(), value.clone())),
                )),
                "remove" => Some(Type::Function(
                    vec![(*key).clone()],
                    Box::new(Type::Map(key.clone(), value.clone())),
                )),
                _ => None,
            },
            Type::Set(inner) => match field {
                "toString" => Some(Type::Function(vec![], Box::new(Type::String))),
                // `s.contains(x)` requires `x` to be the element type, matching
                // `add`/`remove` below: a cross-type membership test such as
                // `%(1 2 3).contains("s")` is rejected instead of typed `Dynamic`.
                "contains" => Some(Type::Function(vec![(*inner).clone()], Box::new(Type::Bool))),
                "isEmpty" => Some(Type::Function(vec![], Box::new(Type::Bool))),
                "size" => Some(Type::Function(vec![], Box::new(Type::Int))),
                "add" => Some(Type::Function(
                    vec![(*inner).clone()],
                    Box::new(Type::Set(inner.clone())),
                )),
                "remove" => Some(Type::Function(
                    vec![(*inner).clone()],
                    Box::new(Type::Set(inner.clone())),
                )),
                "union" | "intersect" | "subtract" => Some(Type::Function(
                    vec![Type::Set(inner.clone())],
                    Box::new(Type::Set(inner.clone())),
                )),
                "toList" => Some(Type::Function(vec![], Box::new(Type::List(inner.clone())))),
                _ => None,
            },
            Type::Var(id) => match field {
                // toString is universal: answer with its type and
                // leave the receiver variable free — unifying it into
                // a structural record used to pin e.g. an Int
                // parameter to `record { toString: ... }`.
                "toString" => Some(Type::Function(vec![], Box::new(Type::String))),
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

fn export_user_enum_schemas(schemas: HashMap<String, EnumSchema>) {
    USER_ENUM_SCHEMAS.with(|known| {
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

fn resolve_user_enum_schemas() -> HashMap<String, EnumSchema> {
    USER_ENUM_SCHEMAS.with(|schemas| schemas.borrow().clone())
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

/// Structural record type returned by `Process#run`:
/// `record { stdout: String; stderr: String; exitCode: Int }`. Built as a
/// closed row so `.stdout`, `.stderr`, and `.exitCode` type-check.
fn process_run_result_type() -> Type {
    let row = Type::RowExtend(
        "exitCode".to_string(),
        Box::new(Type::Int),
        Box::new(Type::RowEmpty),
    );
    let row = Type::RowExtend("stderr".to_string(), Box::new(Type::String), Box::new(row));
    let row = Type::RowExtend("stdout".to_string(), Box::new(Type::String), Box::new(row));
    Type::StructuralRecord(Box::new(row))
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
                "getOrElse",
                Type::Function(
                    vec![Type::Dynamic, Type::Dynamic, Type::Dynamic],
                    Box::new(Type::Dynamic),
                ),
            ),
            // `Map#keys(m)` / `Map#values(m)` are element-typed over fresh
            // generalized variables (the `Type::Var`s are generalized by
            // `free_vars` and instantiated per use), so a key/value flows out
            // as its real type rather than `Dynamic` — e.g.
            // `foldLeft(Map#keys(%["a": 1]))(0)((acc, k) => acc + k)` is now a
            // type error instead of silently folding a String as an Int.
            (
                "keys",
                Type::Function(
                    vec![Type::Map(Box::new(Type::Var(0)), Box::new(Type::Var(1)))],
                    Box::new(Type::List(Box::new(Type::Var(0)))),
                ),
            ),
            (
                "values",
                Type::Function(
                    vec![Type::Map(Box::new(Type::Var(0)), Box::new(Type::Var(1)))],
                    Box::new(Type::List(Box::new(Type::Var(1)))),
                ),
            ),
            (
                "isEmpty",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Bool)),
            ),
            (
                "size",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Int)),
            ),
            (
                "put",
                Type::Function(
                    vec![Type::Dynamic, Type::Dynamic, Type::Dynamic],
                    Box::new(Type::Dynamic),
                ),
            ),
            (
                "remove",
                Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Dynamic)),
            ),
            (
                "fromPairs",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
            ),
            ("empty", Type::Function(vec![], Box::new(Type::Dynamic))),
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
            (
                "add",
                Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Dynamic)),
            ),
            (
                "remove",
                Type::Function(vec![Type::Dynamic, Type::Dynamic], Box::new(Type::Dynamic)),
            ),
            (
                "fromList",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
            ),
            (
                "toList",
                Type::Function(vec![Type::Dynamic], Box::new(Type::Dynamic)),
            ),
            ("empty", Type::Function(vec![], Box::new(Type::Dynamic))),
            // `Set#union` / `intersect` / `subtract` take two sets of the same
            // element type and return a set of that type, over a fresh
            // generalized variable, so mixing element types is a type error
            // instead of producing a `Dynamic`-typed set.
            (
                "union",
                Type::Function(
                    vec![
                        Type::Set(Box::new(Type::Var(0))),
                        Type::Set(Box::new(Type::Var(0))),
                    ],
                    Box::new(Type::Set(Box::new(Type::Var(0)))),
                ),
            ),
            (
                "intersect",
                Type::Function(
                    vec![
                        Type::Set(Box::new(Type::Var(0))),
                        Type::Set(Box::new(Type::Var(0))),
                    ],
                    Box::new(Type::Set(Box::new(Type::Var(0)))),
                ),
            ),
            (
                "subtract",
                Type::Function(
                    vec![
                        Type::Set(Box::new(Type::Var(0))),
                        Type::Set(Box::new(Type::Var(0))),
                    ],
                    Box::new(Type::Set(Box::new(Type::Var(0)))),
                ),
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
        "Process" => &[
            (
                "exit",
                Type::Function(vec![Type::Int], Box::new(Type::Unit)),
            ),
            (
                "run",
                Type::Function(
                    vec![Type::String, Type::List(Box::new(Type::String))],
                    Box::new(process_run_result_type()),
                ),
            ),
        ],
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

/// Rewrite bare `Named` references to a declaration's type parameters
/// into `Type::Generic`, so schema field types substitute correctly at
/// instantiation sites (`v: a` inside `enum Option<a>`).
fn generify_named_params(ty: Type, type_params: &[String]) -> Type {
    match ty {
        Type::Named(name) if type_params.contains(&name) => Type::Generic(name),
        Type::List(inner) => Type::List(Box::new(generify_named_params(*inner, type_params))),
        Type::Set(inner) => Type::Set(Box::new(generify_named_params(*inner, type_params))),
        Type::Map(key, value) => Type::Map(
            Box::new(generify_named_params(*key, type_params)),
            Box::new(generify_named_params(*value, type_params)),
        ),
        Type::Function(params, result) => Type::Function(
            params
                .into_iter()
                .map(|param| generify_named_params(param, type_params))
                .collect(),
            Box::new(generify_named_params(*result, type_params)),
        ),
        Type::Applied(head, args) => Type::Applied(
            Box::new(generify_named_params(*head, type_params)),
            args.into_iter()
                .map(|arg| generify_named_params(arg, type_params))
                .collect(),
        ),
        Type::Record(name, args) => Type::Record(
            name,
            args.into_iter()
                .map(|arg| generify_named_params(arg, type_params))
                .collect(),
        ),
        Type::Enum(name, args) => Type::Enum(
            name,
            args.into_iter()
                .map(|arg| generify_named_params(arg, type_params))
                .collect(),
        ),
        other => other,
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
        "Boolean" | "Bool" => Type::Bool,
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
        "Boolean" | "Bool" => Type::Bool,
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
            // Accept the thin arrow `->` as an alias for `=>` in type
            // annotations, so `(Int) -> Int` parses as a function type
            // instead of an opaque Named type (`-` never appears in an
            // annotation except as part of `->`).
            '-' if paren == 0 && angle == 0 && chars[index + 1].1 == '>' => {
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
        Type::Enum(name, args) => Type::Enum(
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
        Type::Enum(name, args) => Type::Enum(
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

fn collect_var_ids(ty: &Type, type_map: &mut HashMap<u32, u32>, row_map: &mut HashMap<u32, u32>) {
    match ty {
        Type::Var(id) => {
            type_map.entry(*id).or_insert(0);
        }
        Type::RowVar(id) => {
            row_map.entry(*id).or_insert(0);
        }
        Type::List(inner) | Type::Set(inner) | Type::StructuralRecord(inner) => {
            collect_var_ids(inner, type_map, row_map);
        }
        Type::Map(key, value) => {
            collect_var_ids(key, type_map, row_map);
            collect_var_ids(value, type_map, row_map);
        }
        Type::Function(params, result) => {
            for param in params {
                collect_var_ids(param, type_map, row_map);
            }
            collect_var_ids(result, type_map, row_map);
        }
        Type::Record(_, args) | Type::Enum(_, args) => {
            for arg in args {
                collect_var_ids(arg, type_map, row_map);
            }
        }
        Type::Applied(head, args) => {
            collect_var_ids(head, type_map, row_map);
            for arg in args {
                collect_var_ids(arg, type_map, row_map);
            }
        }
        Type::RowExtend(_, field, rest) => {
            collect_var_ids(field, type_map, row_map);
            collect_var_ids(rest, type_map, row_map);
        }
        _ => {}
    }
}

fn rewrite_var_ids(ty: &Type, type_map: &HashMap<u32, u32>, row_map: &HashMap<u32, u32>) -> Type {
    match ty {
        Type::Var(id) => Type::Var(*type_map.get(id).unwrap_or(id)),
        Type::RowVar(id) => Type::RowVar(*row_map.get(id).unwrap_or(id)),
        Type::List(inner) => Type::List(Box::new(rewrite_var_ids(inner, type_map, row_map))),
        Type::Set(inner) => Type::Set(Box::new(rewrite_var_ids(inner, type_map, row_map))),
        Type::StructuralRecord(inner) => {
            Type::StructuralRecord(Box::new(rewrite_var_ids(inner, type_map, row_map)))
        }
        Type::Map(key, value) => Type::Map(
            Box::new(rewrite_var_ids(key, type_map, row_map)),
            Box::new(rewrite_var_ids(value, type_map, row_map)),
        ),
        Type::Function(params, result) => Type::Function(
            params
                .iter()
                .map(|p| rewrite_var_ids(p, type_map, row_map))
                .collect(),
            Box::new(rewrite_var_ids(result, type_map, row_map)),
        ),
        Type::Record(name, args) => Type::Record(
            name.clone(),
            args.iter()
                .map(|a| rewrite_var_ids(a, type_map, row_map))
                .collect(),
        ),
        Type::Enum(name, args) => Type::Enum(
            name.clone(),
            args.iter()
                .map(|a| rewrite_var_ids(a, type_map, row_map))
                .collect(),
        ),
        Type::Applied(head, args) => Type::Applied(
            Box::new(rewrite_var_ids(head, type_map, row_map)),
            args.iter()
                .map(|a| rewrite_var_ids(a, type_map, row_map))
                .collect(),
        ),
        Type::RowExtend(label, field, rest) => Type::RowExtend(
            label.clone(),
            Box::new(rewrite_var_ids(field, type_map, row_map)),
            Box::new(rewrite_var_ids(rest, type_map, row_map)),
        ),
        other => other.clone(),
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
        Type::Enum(expected_name, expected_args) => matches!(
            actual,
            Type::Enum(actual_name, actual_args)
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
        Type::Record(_, args) | Type::Enum(_, args) => {
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

/// Whether a pattern matches every value of its scrutinee type.
fn pattern_is_irrefutable(pattern: &klassic_syntax::Pattern) -> bool {
    matches!(
        pattern,
        klassic_syntax::Pattern::Wildcard { .. } | klassic_syntax::Pattern::Variable { .. }
    )
}

/// A user-facing, type-distinct key for a literal pattern, used to detect a
/// repeated `case` that can never run. The rendering doubles as both the
/// uniqueness key and the diagnostic text, and never collides across types
/// (`` `1` `` for an `Int`, `` `"1"` `` for a `String`). Non-literal
/// patterns return `None`.
fn literal_pattern_key(pattern: &klassic_syntax::Pattern) -> Option<String> {
    use klassic_syntax::Pattern;
    match pattern {
        Pattern::LiteralBool { value, .. } => Some(format!("`{value}`")),
        Pattern::LiteralInt { value, .. } => Some(format!("`{value}`")),
        Pattern::LiteralString { value, .. } => Some(format!("`{value:?}`")),
        _ => None,
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
        Type::Record(_, args) | Type::Enum(_, args) => {
            args.iter().any(|arg| occurs_in(var.clone(), arg))
        }
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

/// Assigns short, readable names (`'a`, `'b`, ...) to the unresolved type
/// and row variables encountered while rendering a single type, so error
/// messages never leak raw internal ids like `'t33` / `'r34`.
#[derive(Default)]
struct TypeVarNamer {
    type_vars: HashMap<u32, String>,
    row_vars: HashMap<u32, String>,
    next: usize,
}

impl TypeVarNamer {
    fn letter(n: usize) -> String {
        let letter = (b'a' + (n % 26) as u8) as char;
        let suffix = n / 26;
        if suffix == 0 {
            format!("'{letter}")
        } else {
            format!("'{letter}{suffix}")
        }
    }

    fn type_name(&mut self, id: u32) -> String {
        if let Some(name) = self.type_vars.get(&id) {
            return name.clone();
        }
        let name = Self::letter(self.next);
        self.next += 1;
        self.type_vars.insert(id, name.clone());
        name
    }

    fn row_name(&mut self, id: u32) -> String {
        if let Some(name) = self.row_vars.get(&id) {
            return name.clone();
        }
        let name = Self::letter(self.next);
        self.next += 1;
        self.row_vars.insert(id, name.clone());
        name
    }
}

fn display_type(ty: &Type) -> String {
    let mut namer = TypeVarNamer::default();
    display_type_named(ty, &mut namer)
}

fn display_type_named(ty: &Type, namer: &mut TypeVarNamer) -> String {
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
        Type::List(inner) => format!("List<{}>", display_type_named(inner, namer)),
        Type::Map(key, value) => format!(
            "Map<{}, {}>",
            display_type_named(key, namer),
            display_type_named(value, namer)
        ),
        Type::Set(inner) => format!("Set<{}>", display_type_named(inner, namer)),
        Type::Function(params, result) => format!(
            "({}) => {}",
            params
                .iter()
                .map(|t| display_type_named(t, namer))
                .collect::<Vec<_>>()
                .join(", "),
            display_type_named(result, namer)
        ),
        Type::Applied(head, args) => format!(
            "{}<{}>",
            display_type_named(head, namer),
            args.iter()
                .map(|t| display_type_named(t, namer))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::Record(name, args) if args.is_empty() => format!("#{name}"),
        Type::Record(name, args) => format!(
            "#{}<{}>",
            name,
            args.iter()
                .map(|t| display_type_named(t, namer))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::Enum(name, args) if args.is_empty() => name.clone(),
        Type::Enum(name, args) => format!(
            "{}<{}>",
            name,
            args.iter()
                .map(|t| display_type_named(t, namer))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::StructuralRecord(row) => format!("record {{ {} }}", display_row_named(row, namer)),
        Type::RowEmpty => String::new(),
        Type::RowExtend(_, _, _) => display_row_named(ty, namer),
        Type::Generic(name) => name.clone(),
        Type::Named(name) => name.clone(),
        Type::Var(id) => namer.type_name(*id),
        Type::RowVar(id) => namer.row_name(*id),
    }
}

fn display_row_named(ty: &Type, namer: &mut TypeVarNamer) -> String {
    match ty {
        Type::RowEmpty => String::new(),
        Type::RowVar(id) => format!("... {}", namer.row_name(*id)),
        Type::RowExtend(label, field, rest) => {
            let mut parts = vec![format!("{label}: {}", display_type_named(field, namer))];
            let rest = display_row_named(rest, namer);
            if !rest.is_empty() {
                parts.push(rest);
            }
            parts.join("; ")
        }
        other => display_type_named(other, namer),
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
        Expr::StringInterpolation { parts, span } => Expr::StringInterpolation {
            parts: parts
                .iter()
                .map(|part| match part {
                    StringPart::Literal(text) => StringPart::Literal(text.clone()),
                    StringPart::Interpolation(hole) => StringPart::Interpolation(Box::new(
                        substitute_expr_identifiers(hole, substitutions),
                    )),
                })
                .collect(),
            span: *span,
        },
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
