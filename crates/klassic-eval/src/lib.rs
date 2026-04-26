use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fmt;
use std::fs;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use klassic_rewrite::rewrite_expression;
use klassic_span::{Diagnostic, DiagnosticKind, Severity, SourceFile, Span};
use klassic_syntax::{
    BinaryOp, Expr, RecordField, TypeAnnotation, TypeClassConstraint, parse_inline_expression,
    parse_source,
};
use klassic_types::{
    clear_user_binding_types, clear_user_typeclass_infos, typecheck_program_with_bindings,
};

mod builtin_registry;
mod builtin_support;
mod ops;
mod runtime_types;
mod value;

use builtin_registry::{builtin_arity, builtin_name, value_method_builtin_name};
use builtin_support::{
    clamp_index, ensure_arity, expect_int, expect_list, expect_map, expect_non_negative_int,
    expect_set, expect_string, simple_regex_is_match, simple_regex_replace_all,
};
use ops::{eval_binary, eval_unary};
use runtime_types::{
    constraint_runtime_type_name, dynamic_type_name, infer_constraint_substitutions,
    known_type_from_value,
};
pub use value::{BuiltinFunctionValue, FunctionValue, Value};

#[derive(Clone, Debug)]
enum ThreadValueSnapshot {
    Int(i64),
    Long(i64),
    Float(f32),
    Double(f64),
    Bool(bool),
    String(String),
    Null,
    Unit,
    List(Vec<ThreadValueSnapshot>),
    Map(Vec<(ThreadValueSnapshot, ThreadValueSnapshot)>),
    Set(Vec<ThreadValueSnapshot>),
    Record {
        name: String,
        fields: Vec<(String, ThreadValueSnapshot)>,
    },
    TypeClassMethod(String),
    BoundTypeClassMethod {
        name: String,
        fallback: Box<ThreadValueSnapshot>,
    },
    BuiltinFunction {
        name: &'static str,
        bound_args: Vec<ThreadValueSnapshot>,
    },
    Function(ThreadFunctionSnapshot),
}

#[derive(Clone, Debug)]
struct ThreadFunctionSnapshot {
    params: Vec<String>,
    param_annotations: Vec<Option<TypeAnnotation>>,
    constraints: Vec<TypeClassConstraint>,
    body: Expr,
    env: ThreadEnvironmentSnapshot,
}

#[derive(Clone, Debug)]
enum ThreadBindingSnapshot {
    Local {
        mutable: bool,
        value: ThreadValueSnapshot,
    },
    Shared {
        mutable: bool,
        value: Arc<Mutex<ThreadValueSnapshot>>,
    },
}

#[derive(Clone, Debug, Default)]
struct ThreadEnvironmentSnapshot {
    scopes: Vec<HashMap<String, ThreadBindingSnapshot>>,
}

#[derive(Clone, Debug)]
struct ThreadInstanceMethodEntrySnapshot {
    for_type: String,
    function: ThreadValueSnapshot,
}

#[derive(Clone, Debug)]
struct ThreadInstanceDictionaryEntrySnapshot {
    for_type: String,
    methods: Vec<(String, ThreadValueSnapshot)>,
}

type ThreadModuleExports = HashMap<String, ThreadValueSnapshot>;
type ThreadInstanceMethods = HashMap<String, Vec<ThreadInstanceMethodEntrySnapshot>>;
type ThreadInstanceDictionaries = HashMap<String, Vec<ThreadInstanceDictionaryEntrySnapshot>>;

#[derive(Clone, Debug)]
pub struct EvaluationError {
    pub source: SourceFile,
    pub diagnostic: Diagnostic,
}

impl EvaluationError {
    pub fn is_incomplete(&self) -> bool {
        self.diagnostic.is_incomplete()
    }
}

impl fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.diagnostic.render(&self.source))
    }
}

impl std::error::Error for EvaluationError {}

#[derive(Clone, Debug)]
struct Binding {
    mutable: bool,
    storage: BindingStorage,
}

type BindingRef = Rc<RefCell<Binding>>;

#[derive(Clone, Debug)]
enum BindingStorage {
    Local(Value),
    Shared(Arc<Mutex<ThreadValueSnapshot>>),
}

impl Binding {
    fn with_value(mutable: bool, value: Value) -> Self {
        Self {
            mutable,
            storage: BindingStorage::Local(value),
        }
    }

    fn placeholder(mutable: bool) -> Self {
        Self::with_value(mutable, Value::Unit)
    }

    fn current_value(&self) -> Value {
        match &self.storage {
            BindingStorage::Local(value) => value.clone(),
            BindingStorage::Shared(cell) => {
                restore_thread_value(cell.lock().expect("shared binding lock").clone())
            }
        }
    }

    fn set_value(&mut self, value: Value) {
        match &mut self.storage {
            BindingStorage::Local(slot) => *slot = value,
            BindingStorage::Shared(cell) => {
                *cell.lock().expect("shared binding lock") = snapshot_value_for_thread(&value);
            }
        }
    }

    fn shared_snapshot_cell(&mut self) -> Arc<Mutex<ThreadValueSnapshot>> {
        match &mut self.storage {
            BindingStorage::Shared(cell) => cell.clone(),
            BindingStorage::Local(value) => {
                let cell = Arc::new(Mutex::new(snapshot_value_for_thread(value)));
                self.storage = BindingStorage::Shared(cell.clone());
                cell
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
struct Environment {
    scopes: Vec<HashMap<String, BindingRef>>,
}

impl Environment {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_with_value(&mut self, name: String, mutable: bool, value: Value) {
        self.declare_binding(
            name,
            Rc::new(RefCell::new(Binding::with_value(mutable, value))),
        );
    }

    fn declare_placeholder(&mut self, name: String, mutable: bool) -> BindingRef {
        let binding = Rc::new(RefCell::new(Binding::placeholder(mutable)));
        self.declare_binding(name, binding.clone());
        binding
    }

    fn declare_binding(&mut self, name: String, binding: BindingRef) {
        self.scopes
            .last_mut()
            .expect("at least one scope always exists")
            .insert(name, binding);
    }

    fn get_binding(&self, name: &str) -> Option<BindingRef> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    fn lookup_value(&self, name: &str) -> Option<Value> {
        self.get_binding(name)
            .map(|binding| binding.borrow().current_value())
    }

    fn assign(&mut self, name: &str, value: Value) -> Result<Value, AssignmentFailure> {
        let Some(binding) = self.get_binding(name) else {
            return Err(AssignmentFailure::Undefined);
        };
        let mut binding = binding.borrow_mut();
        if !binding.mutable {
            return Err(AssignmentFailure::Immutable);
        }
        binding.set_value(value.clone());
        Ok(value)
    }

    fn root_exports(&self) -> HashMap<String, Value> {
        self.scopes
            .first()
            .into_iter()
            .flat_map(|scope| scope.iter())
            .map(|(name, binding)| (name.clone(), binding.borrow().current_value()))
            .collect()
    }
}

fn snapshot_value_for_thread(value: &Value) -> ThreadValueSnapshot {
    match value {
        Value::Int(value) => ThreadValueSnapshot::Int(*value),
        Value::Long(value) => ThreadValueSnapshot::Long(*value),
        Value::Float(value) => ThreadValueSnapshot::Float(*value),
        Value::Double(value) => ThreadValueSnapshot::Double(*value),
        Value::Bool(value) => ThreadValueSnapshot::Bool(*value),
        Value::String(value) => ThreadValueSnapshot::String(value.clone()),
        Value::Null => ThreadValueSnapshot::Null,
        Value::Unit => ThreadValueSnapshot::Unit,
        Value::List(values) => {
            ThreadValueSnapshot::List(values.iter().map(snapshot_value_for_thread).collect())
        }
        Value::Map(entries) => ThreadValueSnapshot::Map(
            entries
                .iter()
                .map(|(key, value)| {
                    (
                        snapshot_value_for_thread(key),
                        snapshot_value_for_thread(value),
                    )
                })
                .collect(),
        ),
        Value::Set(values) => {
            ThreadValueSnapshot::Set(values.iter().map(snapshot_value_for_thread).collect())
        }
        Value::Record { name, fields } => ThreadValueSnapshot::Record {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|(field, value)| (field.clone(), snapshot_value_for_thread(value)))
                .collect(),
        },
        Value::TypeClassMethod(name) => ThreadValueSnapshot::TypeClassMethod(name.clone()),
        Value::BoundTypeClassMethod { name, fallback } => {
            ThreadValueSnapshot::BoundTypeClassMethod {
                name: name.clone(),
                fallback: Box::new(snapshot_value_for_thread(fallback)),
            }
        }
        Value::BuiltinFunction(function) => ThreadValueSnapshot::BuiltinFunction {
            name: function.name,
            bound_args: function
                .bound_args
                .iter()
                .map(snapshot_value_for_thread)
                .collect(),
        },
        Value::Function(function) => ThreadValueSnapshot::Function(ThreadFunctionSnapshot {
            params: function.params.clone(),
            param_annotations: function.param_annotations.clone(),
            constraints: function.constraints.clone(),
            body: function.body.clone(),
            env: snapshot_environment_for_thread(&function.env),
        }),
    }
}

fn restore_thread_value(snapshot: ThreadValueSnapshot) -> Value {
    match snapshot {
        ThreadValueSnapshot::Int(value) => Value::Int(value),
        ThreadValueSnapshot::Long(value) => Value::Long(value),
        ThreadValueSnapshot::Float(value) => Value::Float(value),
        ThreadValueSnapshot::Double(value) => Value::Double(value),
        ThreadValueSnapshot::Bool(value) => Value::Bool(value),
        ThreadValueSnapshot::String(value) => Value::String(value),
        ThreadValueSnapshot::Null => Value::Null,
        ThreadValueSnapshot::Unit => Value::Unit,
        ThreadValueSnapshot::List(values) => {
            Value::List(values.into_iter().map(restore_thread_value).collect())
        }
        ThreadValueSnapshot::Map(entries) => Value::Map(
            entries
                .into_iter()
                .map(|(key, value)| (restore_thread_value(key), restore_thread_value(value)))
                .collect(),
        ),
        ThreadValueSnapshot::Set(values) => {
            Value::Set(values.into_iter().map(restore_thread_value).collect())
        }
        ThreadValueSnapshot::Record { name, fields } => Value::Record {
            name,
            fields: fields
                .into_iter()
                .map(|(field, value)| (field, restore_thread_value(value)))
                .collect(),
        },
        ThreadValueSnapshot::TypeClassMethod(name) => Value::TypeClassMethod(name),
        ThreadValueSnapshot::BoundTypeClassMethod { name, fallback } => {
            Value::BoundTypeClassMethod {
                name,
                fallback: Box::new(restore_thread_value(*fallback)),
            }
        }
        ThreadValueSnapshot::BuiltinFunction { name, bound_args } => {
            Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
                name,
                bound_args: bound_args.into_iter().map(restore_thread_value).collect(),
            }))
        }
        ThreadValueSnapshot::Function(function) => Value::Function(Rc::new(FunctionValue {
            params: function.params,
            param_annotations: function.param_annotations,
            constraints: function.constraints,
            body: function.body,
            env: restore_thread_environment(function.env),
        })),
    }
}

fn snapshot_environment_for_thread(environment: &Environment) -> ThreadEnvironmentSnapshot {
    ThreadEnvironmentSnapshot {
        scopes: environment
            .scopes
            .iter()
            .map(|scope| {
                scope
                    .iter()
                    .map(|(name, binding)| {
                        let mut binding = binding.borrow_mut();
                        (
                            name.clone(),
                            if binding.mutable {
                                ThreadBindingSnapshot::Shared {
                                    mutable: true,
                                    value: binding.shared_snapshot_cell(),
                                }
                            } else {
                                ThreadBindingSnapshot::Local {
                                    mutable: false,
                                    value: snapshot_value_for_thread(&binding.current_value()),
                                }
                            },
                        )
                    })
                    .collect()
            })
            .collect(),
    }
}

fn restore_thread_environment(snapshot: ThreadEnvironmentSnapshot) -> Environment {
    Environment {
        scopes: snapshot
            .scopes
            .into_iter()
            .map(|scope| {
                scope
                    .into_iter()
                    .map(|(name, binding)| {
                        let binding = match binding {
                            ThreadBindingSnapshot::Local { mutable, value } => {
                                Binding::with_value(mutable, restore_thread_value(value))
                            }
                            ThreadBindingSnapshot::Shared { mutable, value } => Binding {
                                mutable,
                                storage: BindingStorage::Shared(value),
                            },
                        };
                        (name, Rc::new(RefCell::new(binding)))
                    })
                    .collect()
            })
            .collect(),
    }
}

fn snapshot_module_exports_for_thread() -> HashMap<String, ThreadModuleExports> {
    USER_MODULES.with(|modules| {
        modules
            .borrow()
            .iter()
            .map(|(module, exports)| {
                (
                    module.clone(),
                    exports
                        .iter()
                        .map(|(name, value)| (name.clone(), snapshot_value_for_thread(value)))
                        .collect(),
                )
            })
            .collect()
    })
}

fn restore_module_exports_for_thread(
    snapshots: HashMap<String, ThreadModuleExports>,
) -> HashMap<String, ModuleExports> {
    snapshots
        .into_iter()
        .map(|(module, exports)| {
            (
                module,
                exports
                    .into_iter()
                    .map(|(name, value)| (name, restore_thread_value(value)))
                    .collect(),
            )
        })
        .collect()
}

fn snapshot_instance_methods_for_thread() -> ThreadInstanceMethods {
    USER_INSTANCES.with(|instances| {
        instances
            .borrow()
            .iter()
            .map(|(name, entries)| {
                (
                    name.clone(),
                    entries
                        .iter()
                        .map(|entry| ThreadInstanceMethodEntrySnapshot {
                            for_type: entry.for_type.clone(),
                            function: snapshot_value_for_thread(&entry.function),
                        })
                        .collect(),
                )
            })
            .collect()
    })
}

fn restore_instance_methods_for_thread(snapshots: ThreadInstanceMethods) -> InstanceMethods {
    snapshots
        .into_iter()
        .map(|(name, entries)| {
            (
                name,
                entries
                    .into_iter()
                    .map(|entry| InstanceMethodEntry {
                        for_type: entry.for_type,
                        function: restore_thread_value(entry.function),
                    })
                    .collect(),
            )
        })
        .collect()
}

fn snapshot_instance_dictionaries_for_thread() -> ThreadInstanceDictionaries {
    USER_INSTANCE_DICTIONARIES.with(|dictionaries| {
        dictionaries
            .borrow()
            .iter()
            .map(|(class_name, entries)| {
                (
                    class_name.clone(),
                    entries
                        .iter()
                        .map(|entry| ThreadInstanceDictionaryEntrySnapshot {
                            for_type: entry.for_type.clone(),
                            methods: entry
                                .methods
                                .iter()
                                .map(|(name, value)| {
                                    (name.clone(), snapshot_value_for_thread(value))
                                })
                                .collect(),
                        })
                        .collect(),
                )
            })
            .collect()
    })
}

fn restore_instance_dictionaries_for_thread(
    snapshots: ThreadInstanceDictionaries,
) -> InstanceDictionaries {
    snapshots
        .into_iter()
        .map(|(class_name, entries)| {
            (
                class_name,
                entries
                    .into_iter()
                    .map(|entry| InstanceDictionaryEntry {
                        for_type: entry.for_type,
                        methods: entry
                            .methods
                            .into_iter()
                            .map(|(name, value)| (name, restore_thread_value(value)))
                            .collect(),
                    })
                    .collect(),
            )
        })
        .collect()
}

fn register_active_thread(handle: thread::JoinHandle<()>) {
    ACTIVE_THREADS.with(|threads| threads.borrow_mut().push(handle));
}

fn join_active_threads() {
    let handles = ACTIVE_THREADS.with(|threads| std::mem::take(&mut *threads.borrow_mut()));
    for handle in handles {
        let _ = handle.join();
    }
}

enum AssignmentFailure {
    Undefined,
    Immutable,
}

type ModuleExports = HashMap<String, Value>;
#[derive(Clone, Debug)]
struct RecordSchema {
    type_params: Vec<String>,
    fields: Vec<RecordFieldSchema>,
}

#[derive(Clone, Debug)]
struct RecordFieldSchema {
    name: String,
    annotation: Option<String>,
}

type InstanceMethods = HashMap<String, Vec<InstanceMethodEntry>>;
type InstanceDictionaries = HashMap<String, Vec<InstanceDictionaryEntry>>;

thread_local! {
    static USER_MODULES: RefCell<HashMap<String, ModuleExports>> = RefCell::new(HashMap::new());
    static USER_RECORDS: RefCell<HashMap<String, RecordSchema>> = RefCell::new(HashMap::new());
    static USER_INSTANCES: RefCell<InstanceMethods> = RefCell::new(HashMap::new());
    static USER_INSTANCE_DICTIONARIES: RefCell<InstanceDictionaries> = RefCell::new(HashMap::new());
    static ACTIVE_THREADS: RefCell<Vec<thread::JoinHandle<()>>> = const { RefCell::new(Vec::new()) };
}

#[derive(Clone, Debug)]
struct InstanceMethodEntry {
    for_type: String,
    function: Value,
}

#[derive(Clone, Debug)]
struct InstanceDictionaryEntry {
    for_type: String,
    methods: HashMap<String, Value>,
}

#[derive(Clone, Debug, Default)]
struct EvaluationState {
    current_module: Option<String>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct EvaluatorConfig {
    pub deny_trust: bool,
    pub warn_trust: bool,
}

#[derive(Clone, Debug, Default)]
pub struct Evaluator {
    environment: Environment,
    config: EvaluatorConfig,
}

impl Evaluator {
    pub fn new() -> Self {
        clear_user_binding_types();
        clear_user_typeclass_infos();
        USER_INSTANCES.with(|instances| instances.borrow_mut().clear());
        USER_INSTANCE_DICTIONARIES.with(|dictionaries| dictionaries.borrow_mut().clear());
        Self {
            environment: Environment::new(),
            config: EvaluatorConfig::default(),
        }
    }

    pub fn with_config(config: EvaluatorConfig) -> Self {
        clear_user_binding_types();
        clear_user_typeclass_infos();
        USER_INSTANCES.with(|instances| instances.borrow_mut().clear());
        USER_INSTANCE_DICTIONARIES.with(|dictionaries| dictionaries.borrow_mut().clear());
        Self {
            environment: Environment::new(),
            config,
        }
    }

    pub fn evaluate_text(&mut self, name: &str, text: &str) -> Result<Value, EvaluationError> {
        let source = SourceFile::new(name, text);
        let expr = parse_source(&source).map_err(|diagnostic| EvaluationError {
            source: source.clone(),
            diagnostic,
        })?;
        let expr = rewrite_expression(expr);
        let known_bindings = self
            .environment
            .root_exports()
            .into_iter()
            .map(|(name, value)| (name, known_type_from_value(&value)))
            .collect::<Vec<_>>();
        typecheck_program_with_bindings(&expr, known_bindings).map_err(|diagnostic| {
            EvaluationError {
                source: source.clone(),
                diagnostic,
            }
        })?;
        analyze_proofs(&expr, self.config).map_err(|diagnostic| EvaluationError {
            source: source.clone(),
            diagnostic,
        })?;
        let mut state = EvaluationState::default();
        let result = eval_program(&expr, &mut self.environment, &mut state);
        join_active_threads();
        let result = result.map_err(|diagnostic| EvaluationError { source, diagnostic })?;
        if let Some(module) = state.current_module {
            export_current_module(&module, &self.environment);
        }
        Ok(result)
    }
}

pub fn evaluate_text(name: &str, text: &str) -> Result<Value, EvaluationError> {
    Evaluator::new().evaluate_text(name, text)
}

pub fn evaluate_text_with_config(
    name: &str,
    text: &str,
    config: EvaluatorConfig,
) -> Result<Value, EvaluationError> {
    Evaluator::with_config(config).evaluate_text(name, text)
}

#[derive(Clone, Debug)]
struct ProofDefinition {
    name: String,
    span: Span,
    proposition: Expr,
    body: Option<Expr>,
    trusted: bool,
    is_axiom: bool,
}

#[derive(Clone, Debug)]
struct ProofMetadata {
    name: String,
    span: Span,
    level: usize,
    trusted: bool,
    dependencies: HashSet<String>,
}

fn analyze_proofs(expr: &Expr, config: EvaluatorConfig) -> Result<(), Diagnostic> {
    let definitions = collect_proof_definitions(expr);
    if definitions.is_empty() {
        return Ok(());
    }
    let metadata = compute_proof_metadata(&definitions)?;
    if config.deny_trust {
        if let Some(proof) = definitions
            .iter()
            .filter_map(|definition| metadata.get(&definition.name))
            .find(|proof| proof.trusted)
        {
            return Err(type_diagnostic(
                proof.span,
                format!(
                    "trusted proof '{}' is not allowed (level {})",
                    proof.name, proof.level
                ),
            ));
        }
    }
    if config.warn_trust {
        let mut proofs = metadata
            .values()
            .filter(|proof| proof.trusted)
            .cloned()
            .collect::<Vec<_>>();
        proofs.sort_by(|lhs, rhs| lhs.name.cmp(&rhs.name));
        for proof in proofs {
            let mut deps = proof.dependencies.iter().cloned().collect::<Vec<_>>();
            deps.sort();
            eprintln!(
                "[trust] proof '{}' is trusted (level {}); depends on [{}]",
                proof.name,
                proof.level,
                deps.join(", ")
            );
        }
    }
    Ok(())
}

fn collect_proof_definitions(expr: &Expr) -> Vec<ProofDefinition> {
    let expressions = match expr {
        Expr::Block { expressions, .. } => expressions.as_slice(),
        other => std::slice::from_ref(other),
    };
    expressions
        .iter()
        .filter_map(|expression| match expression {
            Expr::TheoremDeclaration {
                name,
                proposition,
                body,
                trusted,
                span,
                ..
            } => Some(ProofDefinition {
                name: name.clone(),
                span: *span,
                proposition: proposition.as_ref().clone(),
                body: Some(body.as_ref().clone()),
                trusted: *trusted,
                is_axiom: false,
            }),
            Expr::AxiomDeclaration {
                name,
                proposition,
                span,
                ..
            } => Some(ProofDefinition {
                name: name.clone(),
                span: *span,
                proposition: proposition.as_ref().clone(),
                body: None,
                trusted: true,
                is_axiom: true,
            }),
            _ => None,
        })
        .collect()
}

fn compute_proof_metadata(
    definitions: &[ProofDefinition],
) -> Result<HashMap<String, ProofMetadata>, Diagnostic> {
    let by_name = definitions
        .iter()
        .map(|definition| (definition.name.clone(), definition))
        .collect::<HashMap<_, _>>();
    let proof_names = by_name.keys().cloned().collect::<HashSet<_>>();
    let deps = definitions
        .iter()
        .map(|definition| {
            (
                definition.name.clone(),
                proof_dependencies(definition, &proof_names),
            )
        })
        .collect::<HashMap<_, _>>();

    fn compute_level(
        name: &str,
        by_name: &HashMap<String, &ProofDefinition>,
        deps: &HashMap<String, HashSet<String>>,
        memo: &mut HashMap<String, usize>,
        visiting: &mut HashSet<String>,
    ) -> Result<usize, Diagnostic> {
        if let Some(level) = memo.get(name) {
            return Ok(*level);
        }
        if !visiting.insert(name.to_string()) {
            let span = by_name
                .get(name)
                .map(|definition| definition.span)
                .unwrap_or(Span::new(0, 0));
            return Err(type_diagnostic(
                span,
                format!("cyclic proof dependency detected for '{name}'"),
            ));
        }
        let definition = by_name
            .get(name)
            .expect("proof definition should exist for level computation");
        let base = if definition.is_axiom || definition.trusted {
            1
        } else {
            0
        };
        let mut level = base;
        if let Some(children) = deps.get(name) {
            for dependency in children {
                let dep_level = compute_level(dependency, by_name, deps, memo, visiting)?;
                if dep_level > 0 {
                    level = level.max(dep_level + 1);
                }
            }
        }
        visiting.remove(name);
        memo.insert(name.to_string(), level);
        Ok(level)
    }

    let mut memo = HashMap::new();
    let mut visiting = HashSet::new();
    for definition in definitions {
        compute_level(&definition.name, &by_name, &deps, &mut memo, &mut visiting)?;
    }

    Ok(definitions
        .iter()
        .map(|definition| {
            let level = *memo
                .get(&definition.name)
                .expect("proof level should exist after computation");
            (
                definition.name.clone(),
                ProofMetadata {
                    name: definition.name.clone(),
                    span: definition.span,
                    level,
                    trusted: level > 0,
                    dependencies: deps.get(&definition.name).cloned().unwrap_or_default(),
                },
            )
        })
        .collect())
}

fn proof_dependencies(definition: &ProofDefinition, names: &HashSet<String>) -> HashSet<String> {
    let mut dependencies = HashSet::new();
    collect_referenced_proof_names(&definition.proposition, names, &mut dependencies);
    if let Some(body) = &definition.body {
        collect_referenced_proof_names(body, names, &mut dependencies);
    }
    dependencies.remove(&definition.name);
    dependencies
}

fn collect_referenced_proof_names(
    expr: &Expr,
    names: &HashSet<String>,
    dependencies: &mut HashSet<String>,
) {
    match expr {
        Expr::Identifier { name, .. } => {
            if names.contains(name) {
                dependencies.insert(name.clone());
            }
        }
        Expr::ModuleHeader { .. }
        | Expr::Import { .. }
        | Expr::RecordDeclaration { .. }
        | Expr::RecordLiteral { .. }
        | Expr::TypeClassDeclaration { .. }
        | Expr::PegRuleBlock { .. }
        | Expr::Int { .. }
        | Expr::Double { .. }
        | Expr::Bool { .. }
        | Expr::String { .. }
        | Expr::Null { .. }
        | Expr::Unit { .. } => {}
        Expr::InstanceDeclaration { methods, .. }
        | Expr::Block {
            expressions: methods,
            ..
        } => {
            for method in methods {
                collect_referenced_proof_names(method, names, dependencies);
            }
        }
        Expr::TheoremDeclaration {
            proposition, body, ..
        } => {
            collect_referenced_proof_names(proposition, names, dependencies);
            collect_referenced_proof_names(body, names, dependencies);
        }
        Expr::AxiomDeclaration { proposition, .. } => {
            collect_referenced_proof_names(proposition, names, dependencies);
        }
        Expr::VarDecl { value, .. }
        | Expr::DefDecl { body: value, .. }
        | Expr::Lambda { body: value, .. }
        | Expr::Unary { expr: value, .. }
        | Expr::Cleanup { body: value, .. }
        | Expr::While {
            condition: value, ..
        } => {
            collect_referenced_proof_names(value, names, dependencies);
            if let Expr::Cleanup { cleanup, .. } = expr {
                collect_referenced_proof_names(cleanup, names, dependencies);
            }
            if let Expr::While { body, .. } = expr {
                collect_referenced_proof_names(body, names, dependencies);
            }
        }
        Expr::Assign { value, .. } => collect_referenced_proof_names(value, names, dependencies),
        Expr::Binary { lhs, rhs, .. } => {
            collect_referenced_proof_names(lhs, names, dependencies);
            collect_referenced_proof_names(rhs, names, dependencies);
        }
        Expr::Call {
            callee, arguments, ..
        } => {
            collect_referenced_proof_names(callee, names, dependencies);
            for argument in arguments {
                collect_referenced_proof_names(argument, names, dependencies);
            }
        }
        Expr::FieldAccess { target, .. } => {
            collect_referenced_proof_names(target, names, dependencies);
        }
        Expr::RecordConstructor { arguments, .. }
        | Expr::ListLiteral {
            elements: arguments,
            ..
        }
        | Expr::SetLiteral {
            elements: arguments,
            ..
        } => {
            for argument in arguments {
                collect_referenced_proof_names(argument, names, dependencies);
            }
        }
        Expr::MapLiteral { entries, .. } => {
            for (key, value) in entries {
                collect_referenced_proof_names(key, names, dependencies);
                collect_referenced_proof_names(value, names, dependencies);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_referenced_proof_names(condition, names, dependencies);
            collect_referenced_proof_names(then_branch, names, dependencies);
            if let Some(branch) = else_branch {
                collect_referenced_proof_names(branch, names, dependencies);
            }
        }
        Expr::Foreach { iterable, body, .. } => {
            collect_referenced_proof_names(iterable, names, dependencies);
            collect_referenced_proof_names(body, names, dependencies);
        }
    }
}

fn type_diagnostic(span: Span, message: String) -> Diagnostic {
    Diagnostic {
        kind: DiagnosticKind::Type,
        severity: Severity::Error,
        span: Some(span),
        message,
        incomplete_input: false,
    }
}

fn eval_program(
    expr: &Expr,
    environment: &mut Environment,
    state: &mut EvaluationState,
) -> Result<Value, Diagnostic> {
    match expr {
        Expr::Block { expressions, .. } => eval_sequence(expressions, environment, state),
        _ => eval_expr(expr, environment, state),
    }
}

fn eval_expr(
    expr: &Expr,
    environment: &mut Environment,
    state: &mut EvaluationState,
) -> Result<Value, Diagnostic> {
    match expr {
        Expr::Int { value, kind, .. } => Ok(match kind {
            klassic_syntax::IntLiteralKind::Long => Value::Long(*value),
            _ => Value::Int(*value),
        }),
        Expr::Double { value, kind, .. } => Ok(match kind {
            klassic_syntax::FloatLiteralKind::Float => Value::Float(*value as f32),
            klassic_syntax::FloatLiteralKind::Double => Value::Double(*value),
        }),
        Expr::Bool { value, .. } => Ok(Value::Bool(*value)),
        Expr::String { value, span } => interpolate_string(value, *span, environment),
        Expr::Null { .. } => Ok(Value::Null),
        Expr::Unit { .. } => Ok(Value::Unit),
        Expr::Identifier { name, span } => resolve_identifier(name, *span, environment),
        Expr::ModuleHeader { name, .. } => {
            state.current_module = Some(name.clone());
            Ok(Value::Unit)
        }
        Expr::Import {
            path,
            alias,
            members,
            excludes,
            span,
        } => import_module(
            path,
            alias.as_deref(),
            members.as_deref(),
            excludes,
            *span,
            environment,
        ),
        Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            ..
        } => {
            define_record(name, type_params, fields);
            Ok(Value::Unit)
        }
        Expr::RecordLiteral { fields, .. } => Ok(Value::Record {
            name: String::new(),
            fields: fields
                .iter()
                .map(|(name, value)| Ok((name.clone(), eval_expr(value, environment, state)?)))
                .collect::<Result<Vec<_>, _>>()?,
        }),
        Expr::TypeClassDeclaration { .. } => Ok(Value::Unit),
        Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation: _,
            constraints: _,
            methods,
            span,
        } => {
            define_instance(class_name, for_type, methods, *span, environment)?;
            Ok(Value::Unit)
        }
        Expr::TheoremDeclaration {
            name,
            params,
            proposition,
            ..
        }
        | Expr::AxiomDeclaration {
            name,
            params,
            proposition,
            ..
        } => {
            let placeholder = environment
                .get_binding(name)
                .unwrap_or_else(|| environment.declare_placeholder(name.clone(), false));
            let function = Value::Function(Rc::new(FunctionValue {
                params: params.clone(),
                param_annotations: vec![None; params.len()],
                constraints: Vec::new(),
                body: (*proposition.clone()),
                env: environment.clone(),
            }));
            placeholder.borrow_mut().set_value(function);
            Ok(Value::Unit)
        }
        Expr::PegRuleBlock { .. } => Ok(Value::Unit),
        Expr::VarDecl {
            mutable,
            name,
            value,
            ..
        } => {
            let evaluated = eval_expr(value, environment, state)?;
            environment.declare_with_value(name.clone(), *mutable, evaluated);
            Ok(Value::Unit)
        }
        Expr::DefDecl {
            name,
            param_annotations,
            params,
            body,
            type_params: _,
            constraints,
            ..
        } => {
            let placeholder = environment.declare_placeholder(name.clone(), false);
            let function = Value::Function(Rc::new(FunctionValue {
                params: params.clone(),
                param_annotations: param_annotations.clone(),
                constraints: constraints.clone(),
                body: (*body.clone()),
                env: environment.clone(),
            }));
            placeholder.borrow_mut().set_value(function);
            Ok(Value::Unit)
        }
        Expr::Lambda {
            params,
            param_annotations,
            body,
            ..
        } => Ok(Value::Function(Rc::new(FunctionValue {
            params: params.clone(),
            param_annotations: param_annotations.clone(),
            constraints: Vec::new(),
            body: (*body.clone()),
            env: environment.clone(),
        }))),
        Expr::Assign { name, value, span } => {
            let evaluated = eval_expr(value, environment, state)?;
            match environment.assign(name, evaluated) {
                Ok(value) => Ok(value),
                Err(AssignmentFailure::Undefined) => Err(Diagnostic::runtime(
                    *span,
                    format!("undefined variable `{name}`"),
                )),
                Err(AssignmentFailure::Immutable) => Err(Diagnostic::runtime(
                    *span,
                    format!("cannot assign to immutable binding `{name}`"),
                )),
            }
        }
        Expr::Unary { op, expr, span } => {
            let value = eval_expr(expr, environment, state)?;
            eval_unary(*op, value, *span)
        }
        Expr::Binary { lhs, op, rhs, span } => {
            if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
                return eval_logical(*op, lhs, rhs, *span, environment, state);
            }
            let lhs_value = eval_expr(lhs, environment, state)?;
            let rhs_value = eval_expr(rhs, environment, state)?;
            eval_binary(*op, lhs_value, rhs_value, *span)
        }
        Expr::Call {
            callee,
            arguments,
            span,
        } => eval_call(callee, arguments, *span, environment, state),
        Expr::FieldAccess {
            target,
            field,
            span,
        } => {
            let target = eval_expr(target, environment, state)?;
            match target {
                record @ Value::Record { .. } => {
                    let fields = match &record {
                        Value::Record { fields, .. } => fields,
                        _ => unreachable!("record pattern should have matched"),
                    };
                    let value = fields
                        .iter()
                        .find(|(name, _)| name == field)
                        .map(|(_, value)| value.clone())
                        .ok_or_else(|| {
                            Diagnostic::runtime(*span, format!("record has no field `{field}`"))
                        })?;
                    if matches!(
                        &value,
                        Value::Function(function)
                            if function.params.first().is_some_and(|param| param == "this")
                    ) {
                        partially_apply_callable(value, vec![record], *span)
                    } else {
                        Ok(value)
                    }
                }
                _ => Err(Diagnostic::runtime(*span, "field access expects a record")),
            }
        }
        Expr::Cleanup { body, cleanup, .. } => {
            let body_result = eval_expr(body, environment, state);
            let cleanup_result = eval_expr(cleanup, environment, state);
            match (body_result, cleanup_result) {
                (_, Err(error)) => Err(error),
                (Err(error), _) => Err(error),
                (Ok(value), Ok(_)) => Ok(value),
            }
        }
        Expr::RecordConstructor {
            name,
            arguments,
            span,
        } => {
            let fields = resolve_record(name)
                .ok_or_else(|| Diagnostic::runtime(*span, format!("unknown record `{name}`")))?;
            if fields.fields.len() != arguments.len() {
                return Err(Diagnostic::runtime(
                    *span,
                    format!(
                        "record `{name}` expects {} arguments but got {}",
                        fields.fields.len(),
                        arguments.len()
                    ),
                ));
            }
            let values = arguments
                .iter()
                .map(|argument| eval_expr(argument, environment, state))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::Record {
                name: name.clone(),
                fields: fields
                    .fields
                    .into_iter()
                    .map(|field| field.name)
                    .zip(values)
                    .collect(),
            })
        }
        Expr::ListLiteral { elements, .. } => Ok(Value::List(
            elements
                .iter()
                .map(|element| eval_expr(element, environment, state))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        Expr::MapLiteral { entries, .. } => Ok(Value::Map(
            entries
                .iter()
                .map(|(key, value)| {
                    Ok((
                        eval_expr(key, environment, state)?,
                        eval_expr(value, environment, state)?,
                    ))
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?,
        )),
        Expr::SetLiteral { elements, .. } => {
            let mut values = Vec::new();
            for element in elements {
                let value = eval_expr(element, environment, state)?;
                if !values.contains(&value) {
                    values.push(value);
                }
            }
            Ok(Value::Set(values))
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => match eval_expr(condition, environment, state)? {
            Value::Bool(true) => eval_expr(then_branch, environment, state),
            Value::Bool(false) => match else_branch {
                Some(branch) => eval_expr(branch, environment, state),
                None => Ok(Value::Unit),
            },
            _ => Err(Diagnostic::runtime(
                *span,
                "if condition must evaluate to a boolean",
            )),
        },
        Expr::While {
            condition,
            body,
            span,
        } => loop {
            match eval_expr(condition, environment, state)? {
                Value::Bool(true) => {
                    let _ = eval_expr(body, environment, state)?;
                }
                Value::Bool(false) => return Ok(Value::Unit),
                _ => {
                    return Err(Diagnostic::runtime(
                        *span,
                        "while condition must evaluate to a boolean",
                    ));
                }
            }
        },
        Expr::Foreach {
            binding,
            iterable,
            body,
            span,
        } => {
            let iterable = eval_expr(iterable, environment, state)?;
            let Value::List(elements) = iterable else {
                return Err(Diagnostic::runtime(*span, "foreach expects a list"));
            };
            for element in elements {
                environment.push_scope();
                environment.declare_with_value(binding.clone(), false, element);
                let result = eval_expr(body, environment, state);
                environment.pop_scope();
                result?;
            }
            Ok(Value::Unit)
        }
        Expr::Block { expressions, .. } => {
            environment.push_scope();
            let result = eval_sequence(expressions, environment, state);
            environment.pop_scope();
            result
        }
    }
}

fn eval_sequence(
    expressions: &[Expr],
    environment: &mut Environment,
    state: &mut EvaluationState,
) -> Result<Value, Diagnostic> {
    predeclare_proof_placeholders(expressions, environment);
    let mut last = Value::Unit;
    for expression in expressions {
        last = eval_expr(expression, environment, state)?;
    }
    Ok(last)
}

fn predeclare_proof_placeholders(expressions: &[Expr], environment: &mut Environment) {
    for expression in expressions {
        match expression {
            Expr::TheoremDeclaration { name, .. } | Expr::AxiomDeclaration { name, .. } => {
                if environment.get_binding(name).is_none() {
                    environment.declare_placeholder(name.clone(), false);
                }
            }
            _ => {}
        }
    }
}

fn resolve_identifier(
    name: &str,
    span: Span,
    environment: &Environment,
) -> Result<Value, Diagnostic> {
    if let Some(value) = environment.lookup_value(name) {
        return Ok(value);
    }
    if let Some(function_name) = builtin_name(name) {
        return Ok(Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
            name: function_name,
            bound_args: Vec::new(),
        })));
    }
    if let Some(value) = resolve_module_selector(name) {
        return Ok(value);
    }
    if has_instance_method(name) {
        return Ok(Value::TypeClassMethod(name.to_string()));
    }
    Err(Diagnostic::runtime(
        span,
        format!("undefined variable `{name}`"),
    ))
}

fn import_module(
    path: &str,
    alias: Option<&str>,
    members: Option<&[String]>,
    excludes: &[String],
    span: Span,
    environment: &mut Environment,
) -> Result<Value, Diagnostic> {
    let exports = module_exports(path)
        .ok_or_else(|| Diagnostic::runtime(span, format!("unknown module `{path}`")))?;

    if let Some(alias) = alias {
        for (name, value) in &exports {
            if excludes.iter().any(|member| member == name) {
                continue;
            }
            if members.is_some_and(|allowed| !allowed.iter().any(|member| member == name)) {
                continue;
            }
            environment.declare_with_value(format!("{alias}#{name}"), false, value.clone());
        }
    }

    match members {
        Some(members) => {
            for member in members {
                if excludes.iter().any(|excluded| excluded == member) {
                    continue;
                }
                let value = exports.get(member).cloned().ok_or_else(|| {
                    Diagnostic::runtime(span, format!("module `{path}` has no member `{member}`"))
                })?;
                environment.declare_with_value(member.clone(), false, value);
            }
        }
        None if alias.is_none() => {
            for (name, value) in exports {
                if excludes.iter().any(|member| member == &name) {
                    continue;
                }
                environment.declare_with_value(name, false, value);
            }
        }
        None => {}
    }

    Ok(Value::Unit)
}

fn export_current_module(name: &str, environment: &Environment) {
    USER_MODULES.with(|modules| {
        modules
            .borrow_mut()
            .insert(name.to_string(), environment.root_exports());
    });
}

fn module_exports(path: &str) -> Option<ModuleExports> {
    builtin_module_exports(path).or_else(|| resolve_user_module(path))
}

fn builtin_module_exports(path: &str) -> Option<ModuleExports> {
    let members = builtin_module_members(path)?;
    let mut exports = HashMap::new();
    for member in members {
        let full_name = format!("{path}#{member}");
        let builtin = builtin_name(&full_name)?;
        exports.insert(
            (*member).to_string(),
            Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
                name: builtin,
                bound_args: Vec::new(),
            })),
        );
    }
    Some(exports)
}

fn builtin_module_members(path: &str) -> Option<&'static [&'static str]> {
    match path {
        "FileInput" => Some(&["open", "readAll", "readLines", "all", "lines"]),
        "FileOutput" => Some(&["write", "append", "exists", "delete", "writeLines"]),
        "Dir" => Some(&[
            "current",
            "home",
            "temp",
            "exists",
            "mkdir",
            "mkdirs",
            "isDirectory",
            "isFile",
            "list",
            "listFull",
            "delete",
            "copy",
            "move",
        ]),
        "Map" => Some(&["containsKey", "containsValue", "get", "isEmpty", "size"]),
        "Set" => Some(&["contains", "isEmpty", "size"]),
        _ => None,
    }
}

fn resolve_user_module(path: &str) -> Option<ModuleExports> {
    USER_MODULES.with(|modules| {
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

fn resolve_module_selector(name: &str) -> Option<Value> {
    let (module, member) = name.split_once('#')?;
    module_exports(module)?.get(member).cloned()
}

fn define_record(name: &str, type_params: &[String], fields: &[RecordField]) {
    USER_RECORDS.with(|records| {
        records.borrow_mut().insert(
            name.to_string(),
            RecordSchema {
                type_params: type_params.to_vec(),
                fields: fields
                    .iter()
                    .map(|field| RecordFieldSchema {
                        name: field.name.clone(),
                        annotation: field
                            .annotation
                            .as_ref()
                            .map(|annotation| annotation.text.clone()),
                    })
                    .collect(),
            },
        );
    });
}

fn resolve_record(name: &str) -> Option<RecordSchema> {
    builtin_record_fields(name)
        .or_else(|| USER_RECORDS.with(|records| records.borrow().get(name).cloned()))
}

fn builtin_record_fields(name: &str) -> Option<RecordSchema> {
    match name {
        "Point" => Some(RecordSchema {
            type_params: Vec::new(),
            fields: vec![
                RecordFieldSchema {
                    name: "x".to_string(),
                    annotation: Some("Int".to_string()),
                },
                RecordFieldSchema {
                    name: "y".to_string(),
                    annotation: Some("Int".to_string()),
                },
            ],
        }),
        _ => None,
    }
}

fn define_instance(
    class_name: &str,
    for_type: &str,
    methods: &[Expr],
    span: Span,
    environment: &Environment,
) -> Result<(), Diagnostic> {
    for method in methods {
        let Expr::DefDecl { .. } = method else {
            return Err(Diagnostic::runtime(
                span,
                "instance methods must be function declarations",
            ));
        };
    }

    let mut entries = Vec::new();
    for method in methods {
        let Expr::DefDecl {
            name,
            params,
            param_annotations,
            body,
            type_params: _,
            constraints,
            ..
        } = method
        else {
            continue;
        };
        let function = Value::Function(Rc::new(FunctionValue {
            params: params.clone(),
            param_annotations: param_annotations.clone(),
            constraints: constraints.clone(),
            body: (*body.clone()),
            env: environment.clone(),
        }));
        entries.push((name.clone(), function));
    }

    let dictionary_methods = entries.clone().into_iter().collect::<HashMap<_, _>>();
    USER_INSTANCES.with(|instances| {
        let mut instances = instances.borrow_mut();
        for (method_name, function) in entries {
            let candidates = instances.entry(method_name).or_default();
            if let Some(existing) = candidates
                .iter_mut()
                .find(|entry| entry.for_type == for_type)
            {
                existing.function = function;
            } else {
                candidates.push(InstanceMethodEntry {
                    for_type: for_type.to_string(),
                    function,
                });
            }
        }
    });
    USER_INSTANCE_DICTIONARIES.with(|dictionaries| {
        let mut dictionaries = dictionaries.borrow_mut();
        let entry = InstanceDictionaryEntry {
            for_type: for_type.to_string(),
            methods: dictionary_methods,
        };
        let candidates = dictionaries.entry(class_name.to_string()).or_default();
        if let Some(existing) = candidates
            .iter_mut()
            .find(|candidate| candidate.for_type == for_type)
        {
            *existing = entry;
        } else {
            candidates.push(entry);
        }
    });
    let _ = class_name;
    Ok(())
}

fn dispatch_instance_method(
    name: &str,
    argument_values: &[Value],
    span: Span,
) -> Result<Option<Value>, Diagnostic> {
    let maybe_entry = USER_INSTANCES.with(|instances| {
        let instances = instances.borrow();
        let candidates = instances.get(name)?;
        for argument in argument_values {
            let Some(type_name) = dynamic_type_name(argument) else {
                continue;
            };
            if let Some(entry) = candidates
                .iter()
                .rev()
                .find(|entry| entry.for_type == type_name)
            {
                return Some(entry.clone());
            }
        }
        None
    });

    if let Some(entry) = maybe_entry {
        return Ok(Some(apply_callable(
            entry.function,
            argument_values.to_vec(),
            span,
        )?));
    }
    Ok(None)
}

fn has_instance_method(name: &str) -> bool {
    USER_INSTANCES.with(|instances| instances.borrow().contains_key(name))
}

fn resolve_instance_dictionary(
    class_name: &str,
    type_name: &str,
) -> Option<HashMap<String, Value>> {
    USER_INSTANCE_DICTIONARIES.with(|dictionaries| {
        dictionaries
            .borrow()
            .get(class_name)?
            .iter()
            .rev()
            .find(|entry| entry.for_type == type_name)
            .map(|entry| entry.methods.clone())
    })
}

#[cfg(test)]
fn clear_user_modules() {
    USER_MODULES.with(|modules| modules.borrow_mut().clear());
    klassic_types::clear_user_module_types();
}

#[cfg(test)]
fn clear_user_records() {
    USER_RECORDS.with(|records| records.borrow_mut().clear());
    klassic_types::clear_user_record_schemas();
}

#[cfg(test)]
fn clear_user_instances() {
    USER_INSTANCES.with(|instances| instances.borrow_mut().clear());
    USER_INSTANCE_DICTIONARIES.with(|dictionaries| dictionaries.borrow_mut().clear());
}

fn eval_logical(
    op: BinaryOp,
    lhs: &Expr,
    rhs: &Expr,
    span: Span,
    environment: &mut Environment,
    state: &mut EvaluationState,
) -> Result<Value, Diagnostic> {
    let lhs_value = eval_expr(lhs, environment, state)?;
    match (op, lhs_value) {
        (BinaryOp::LogicalAnd, Value::Bool(false)) => Ok(Value::Bool(false)),
        (BinaryOp::LogicalAnd, Value::Bool(true)) => match eval_expr(rhs, environment, state)? {
            Value::Bool(value) => Ok(Value::Bool(value)),
            _ => Err(Diagnostic::runtime(
                span,
                "logical operators require booleans",
            )),
        },
        (BinaryOp::LogicalOr, Value::Bool(true)) => Ok(Value::Bool(true)),
        (BinaryOp::LogicalOr, Value::Bool(false)) => match eval_expr(rhs, environment, state)? {
            Value::Bool(value) => Ok(Value::Bool(value)),
            _ => Err(Diagnostic::runtime(
                span,
                "logical operators require booleans",
            )),
        },
        _ => Err(Diagnostic::runtime(
            span,
            "logical operators require booleans",
        )),
    }
}

fn eval_call(
    callee: &Expr,
    arguments: &[Expr],
    span: Span,
    environment: &mut Environment,
    state: &mut EvaluationState,
) -> Result<Value, Diagnostic> {
    if let Expr::FieldAccess { target, field, .. } = callee {
        let target_value = eval_expr(target, environment, state)?;
        if let Some(builtin) = value_method_builtin_name(&target_value, field) {
            let mut all_arguments = Vec::with_capacity(arguments.len() + 1);
            all_arguments.push(target_value);
            all_arguments.extend(
                arguments
                    .iter()
                    .map(|argument| eval_expr(argument, environment, state))
                    .collect::<Result<Vec<_>, _>>()?,
            );
            return eval_builtin(builtin, &all_arguments, span);
        }
    }

    let argument_values = arguments
        .iter()
        .map(|argument| eval_expr(argument, environment, state))
        .collect::<Result<Vec<_>, _>>()?;

    if let Expr::Identifier { name, .. } = callee {
        let prefer_instance = prefer_instance_dispatch(name, &argument_values);
        let builtin_or_module_available =
            builtin_name(name).is_some() || resolve_module_selector(name).is_some();
        if environment.get_binding(name).is_none()
            && (prefer_instance || !builtin_or_module_available)
        {
            if let Some(value) = dispatch_instance_method(name, &argument_values, span)? {
                return Ok(value);
            }
        }
    }

    let callee_value = eval_expr(callee, environment, state)?;
    apply_callable(callee_value, argument_values, span)
}

fn apply_callable(
    callee_value: Value,
    argument_values: Vec<Value>,
    span: Span,
) -> Result<Value, Diagnostic> {
    match callee_value {
        Value::TypeClassMethod(name) => dispatch_instance_method(&name, &argument_values, span)?
            .ok_or_else(|| {
                Diagnostic::runtime(
                    span,
                    format!("no matching instance method `{name}` for given arguments"),
                )
            }),
        Value::BoundTypeClassMethod { name, fallback } => {
            if let Some(value) = dispatch_instance_method(&name, &argument_values, span)? {
                Ok(value)
            } else {
                apply_callable(*fallback, argument_values, span)
            }
        }
        Value::Function(function) => invoke_user_function(&function, argument_values, span),
        Value::BuiltinFunction(function) => {
            invoke_builtin_function(&function, argument_values, span)
        }
        _ => Err(Diagnostic::runtime(span, "callee is not a function")),
    }
}

fn partially_apply_callable(
    callee_value: Value,
    argument_values: Vec<Value>,
    span: Span,
) -> Result<Value, Diagnostic> {
    match callee_value {
        Value::TypeClassMethod(name) => {
            if argument_values.is_empty() {
                return Ok(Value::TypeClassMethod(name));
            }
            Err(Diagnostic::runtime(
                span,
                "typeclass methods do not support partial application",
            ))
        }
        Value::BoundTypeClassMethod { name, fallback } => {
            if argument_values.is_empty() {
                return Ok(Value::BoundTypeClassMethod { name, fallback });
            }
            Err(Diagnostic::runtime(
                span,
                "typeclass methods do not support partial application",
            ))
        }
        Value::Function(function) => {
            if argument_values.len() > function.params.len() {
                return Err(Diagnostic::runtime(
                    span,
                    format!(
                        "expected {} arguments but got {}",
                        function.params.len(),
                        argument_values.len()
                    ),
                ));
            }
            let bound_count = argument_values.len();
            let mut partial_env = function.env.clone();
            partial_env.push_scope();
            for (param, value) in function.params.iter().zip(argument_values.into_iter()) {
                partial_env.declare_with_value(param.clone(), false, value);
            }
            Ok(Value::Function(Rc::new(FunctionValue {
                params: function.params[bound_count..].to_vec(),
                param_annotations: function.param_annotations[bound_count..].to_vec(),
                constraints: function.constraints.clone(),
                body: function.body.clone(),
                env: partial_env,
            })))
        }
        Value::BuiltinFunction(function) => {
            let mut all_args = function.bound_args.clone();
            all_args.extend(argument_values);
            let arity = builtin_arity(function.name).expect("builtin exists");
            if all_args.len() > arity {
                return Err(Diagnostic::runtime(
                    span,
                    format!(
                        "{} expects {} arguments but got {}",
                        function.name,
                        arity,
                        all_args.len()
                    ),
                ));
            }
            Ok(Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
                name: function.name,
                bound_args: all_args,
            })))
        }
        _ => Err(Diagnostic::runtime(span, "callee is not a function")),
    }
}

fn invoke_user_function(
    function: &FunctionValue,
    argument_values: Vec<Value>,
    span: Span,
) -> Result<Value, Diagnostic> {
    if argument_values.len() != function.params.len() {
        return Err(Diagnostic::runtime(
            span,
            format!(
                "expected {} arguments but got {}",
                function.params.len(),
                argument_values.len()
            ),
        ));
    }

    let mut call_env = function.env.clone();
    call_env.push_scope();
    for (param, value) in function.params.iter().zip(argument_values.iter().cloned()) {
        call_env.declare_with_value(param.clone(), false, value);
    }
    bind_constraint_methods_for_call(function, &argument_values, &mut call_env, span)?;
    let mut state = EvaluationState::default();
    let result = eval_expr(&function.body, &mut call_env, &mut state);
    call_env.pop_scope();
    result
}

fn bind_constraint_methods_for_call(
    function: &FunctionValue,
    argument_values: &[Value],
    environment: &mut Environment,
    span: Span,
) -> Result<(), Diagnostic> {
    if function.constraints.is_empty() {
        return Ok(());
    }
    let substitutions =
        infer_constraint_substitutions(&function.param_annotations, argument_values);
    for constraint in &function.constraints {
        let Some(type_name) = constraint_runtime_type_name(constraint, &substitutions) else {
            return Err(Diagnostic::runtime(
                span,
                format!(
                    "cannot determine runtime instance for constraint `{}`",
                    constraint.class_name
                ),
            ));
        };
        let methods =
            resolve_instance_dictionary(&constraint.class_name, &type_name).ok_or_else(|| {
                Diagnostic::runtime(
                    span,
                    format!(
                        "no matching instance dictionary `{}` for runtime type `{}`",
                        constraint.class_name, type_name
                    ),
                )
            })?;
        for (name, value) in methods {
            environment.declare_with_value(
                name.clone(),
                false,
                Value::BoundTypeClassMethod {
                    name,
                    fallback: Box::new(value),
                },
            );
        }
    }
    Ok(())
}

fn invoke_builtin_function(
    function: &BuiltinFunctionValue,
    argument_values: Vec<Value>,
    span: Span,
) -> Result<Value, Diagnostic> {
    let mut all_args = function.bound_args.clone();
    all_args.extend(argument_values);
    let arity = builtin_arity(function.name).expect("builtin exists");
    if all_args.len() < arity {
        return Ok(Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
            name: function.name,
            bound_args: all_args,
        })));
    }
    if all_args.len() > arity {
        return Err(Diagnostic::runtime(
            span,
            format!(
                "{} expects {} arguments but got {}",
                function.name,
                arity,
                all_args.len()
            ),
        ));
    }
    eval_builtin(function.name, &all_args, span)
}

fn eval_builtin(name: &str, arguments: &[Value], span: Span) -> Result<Value, Diagnostic> {
    match name {
        "println" => {
            ensure_arity(name, arguments, 1, span)?;
            println!("{}", arguments[0]);
            Ok(Value::Unit)
        }
        "printlnError" => {
            ensure_arity(name, arguments, 1, span)?;
            eprintln!("{}", arguments[0]);
            Ok(Value::Unit)
        }
        "ToDo" => {
            ensure_arity(name, arguments, 0, span)?;
            Err(Diagnostic::runtime(span, "not implemented yet"))
        }
        "assert" => {
            ensure_arity(name, arguments, 1, span)?;
            match &arguments[0] {
                Value::Bool(true) => Ok(Value::Unit),
                Value::Bool(false) => Err(Diagnostic::runtime(span, "assertion failed")),
                _ => Err(Diagnostic::runtime(span, "assert expects a boolean")),
            }
        }
        "assertResult" => {
            ensure_arity(name, arguments, 2, span)?;
            if arguments[0] == arguments[1] {
                Ok(Value::Unit)
            } else {
                Err(Diagnostic::runtime(
                    span,
                    format!(
                        "assertResult failed: expected {} but got {}",
                        arguments[0], arguments[1]
                    ),
                ))
            }
        }
        "sleep" => {
            ensure_arity(name, arguments, 1, span)?;
            let millis = expect_non_negative_int(&arguments[0], "sleep", span)? as u64;
            thread::sleep(Duration::from_millis(millis));
            Ok(Value::Unit)
        }
        "thread" => {
            ensure_arity(name, arguments, 1, span)?;
            let callable = snapshot_value_for_thread(&arguments[0]);
            let module_exports = snapshot_module_exports_for_thread();
            let record_schemas = USER_RECORDS.with(|records| records.borrow().clone());
            let instance_methods = snapshot_instance_methods_for_thread();
            let instance_dictionaries = snapshot_instance_dictionaries_for_thread();
            let handle = thread::spawn(move || {
                USER_MODULES.with(|modules| {
                    *modules.borrow_mut() = restore_module_exports_for_thread(module_exports);
                });
                USER_RECORDS.with(|records| {
                    *records.borrow_mut() = record_schemas;
                });
                USER_INSTANCES.with(|instances| {
                    *instances.borrow_mut() = restore_instance_methods_for_thread(instance_methods);
                });
                USER_INSTANCE_DICTIONARIES.with(|dictionaries| {
                    *dictionaries.borrow_mut() =
                        restore_instance_dictionaries_for_thread(instance_dictionaries);
                });
                let callable = restore_thread_value(callable);
                let _ = apply_callable(callable, Vec::new(), span);
            });
            register_active_thread(handle);
            Ok(Value::Unit)
        }
        "stopwatch" => {
            ensure_arity(name, arguments, 1, span)?;
            let start = Instant::now();
            let _ = apply_callable(arguments[0].clone(), Vec::new(), span)?;
            Ok(Value::Int(start.elapsed().as_millis() as i64))
        }
        "double" => {
            ensure_arity(name, arguments, 1, span)?;
            match &arguments[0] {
                Value::Int(value) => Ok(Value::Double(*value as f64)),
                Value::Long(value) => Ok(Value::Double(*value as f64)),
                Value::Float(value) => Ok(Value::Double(*value as f64)),
                Value::Double(value) => Ok(Value::Double(*value)),
                _ => Err(Diagnostic::runtime(span, "double expects a number")),
            }
        }
        "int" | "floor" => {
            ensure_arity(name, arguments, 1, span)?;
            match &arguments[0] {
                Value::Int(value) => Ok(Value::Int(*value)),
                Value::Long(value) => Ok(Value::Int(*value)),
                Value::Float(value) => Ok(Value::Int(value.trunc() as i64)),
                Value::Double(value) => Ok(Value::Int(value.trunc() as i64)),
                _ => Err(Diagnostic::runtime(
                    span,
                    format!("{name} expects a number"),
                )),
            }
        }
        "ceil" => {
            ensure_arity(name, arguments, 1, span)?;
            match &arguments[0] {
                Value::Int(value) => Ok(Value::Int(*value)),
                Value::Long(value) => Ok(Value::Int(*value)),
                Value::Float(value) => Ok(Value::Int(value.ceil() as i64)),
                Value::Double(value) => Ok(Value::Int(value.ceil() as i64)),
                _ => Err(Diagnostic::runtime(span, "ceil expects a number")),
            }
        }
        "abs" => {
            ensure_arity(name, arguments, 1, span)?;
            match &arguments[0] {
                Value::Int(value) => value
                    .checked_abs()
                    .map(Value::Int)
                    .ok_or_else(|| Diagnostic::runtime(span, "integer overflow")),
                Value::Long(value) => value
                    .checked_abs()
                    .map(Value::Long)
                    .ok_or_else(|| Diagnostic::runtime(span, "integer overflow")),
                Value::Float(value) => Ok(Value::Float(value.abs())),
                Value::Double(value) => Ok(Value::Double(value.abs())),
                _ => Err(Diagnostic::runtime(span, "abs expects a number")),
            }
        }
        "sqrt" => {
            ensure_arity(name, arguments, 1, span)?;
            match &arguments[0] {
                Value::Int(value) => Ok(Value::Double((*value as f64).sqrt())),
                Value::Long(value) => Ok(Value::Double((*value as f64).sqrt())),
                Value::Float(value) => Ok(Value::Double((*value as f64).sqrt())),
                Value::Double(value) => Ok(Value::Double(value.sqrt())),
                _ => Err(Diagnostic::runtime(span, "sqrt expects a number")),
            }
        }
        "toString" => {
            ensure_arity(name, arguments, 1, span)?;
            Ok(Value::String(arguments[0].to_string()))
        }
        "substring" => {
            ensure_arity(name, arguments, 3, span)?;
            let input = expect_string(&arguments[0], "substring", span)?;
            let start = expect_int(&arguments[1], "substring", span)?;
            let end = expect_int(&arguments[2], "substring", span)?;
            let chars = input.chars().collect::<Vec<_>>();
            let start = clamp_index(start, chars.len());
            let end = clamp_index(end, chars.len()).max(start);
            Ok(Value::String(chars[start..end].iter().collect()))
        }
        "at" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "at", span)?;
            let index = expect_int(&arguments[1], "at", span)?;
            let chars = input.chars().collect::<Vec<_>>();
            let index = clamp_index(index, chars.len());
            let value = chars.get(index).copied().unwrap_or_default();
            Ok(Value::String(if value == '\0' {
                String::new()
            } else {
                value.to_string()
            }))
        }
        "matches" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "matches", span)?;
            let pattern = expect_string(&arguments[1], "matches", span)?;
            Ok(Value::Bool(simple_regex_is_match(input, pattern)))
        }
        "split" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "split", span)?;
            let delimiter = expect_string(&arguments[1], "split", span)?;
            Ok(Value::List(if delimiter.is_empty() {
                input
                    .chars()
                    .map(|ch| Value::String(ch.to_string()))
                    .collect()
            } else {
                input
                    .split(delimiter)
                    .map(|part| Value::String(part.to_string()))
                    .collect()
            }))
        }
        "join" => {
            ensure_arity(name, arguments, 2, span)?;
            let list = expect_list(&arguments[0], "join", span)?;
            let delimiter = expect_string(&arguments[1], "join", span)?;
            let parts = list
                .iter()
                .map(|value| match value {
                    Value::String(text) => Ok(text.clone()),
                    _ => Err(Diagnostic::runtime(span, "join expects a list of strings")),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::String(parts.join(delimiter)))
        }
        "trim" => {
            ensure_arity(name, arguments, 1, span)?;
            let input = expect_string(&arguments[0], "trim", span)?;
            Ok(Value::String(input.trim().to_string()))
        }
        "trimLeft" => {
            ensure_arity(name, arguments, 1, span)?;
            let input = expect_string(&arguments[0], "trimLeft", span)?;
            Ok(Value::String(input.trim_start().to_string()))
        }
        "trimRight" => {
            ensure_arity(name, arguments, 1, span)?;
            let input = expect_string(&arguments[0], "trimRight", span)?;
            Ok(Value::String(input.trim_end().to_string()))
        }
        "replace" => {
            ensure_arity(name, arguments, 3, span)?;
            let input = expect_string(&arguments[0], "replace", span)?;
            let from = expect_string(&arguments[1], "replace", span)?;
            let to = expect_string(&arguments[2], "replace", span)?;
            Ok(Value::String(input.replacen(from, to, 1)))
        }
        "replaceAll" => {
            ensure_arity(name, arguments, 3, span)?;
            let input = expect_string(&arguments[0], "replaceAll", span)?;
            let pattern = expect_string(&arguments[1], "replaceAll", span)?;
            let replacement = expect_string(&arguments[2], "replaceAll", span)?;
            Ok(Value::String(simple_regex_replace_all(
                input,
                pattern,
                replacement,
            )))
        }
        "toLowerCase" => {
            ensure_arity(name, arguments, 1, span)?;
            let input = expect_string(&arguments[0], "toLowerCase", span)?;
            Ok(Value::String(input.to_lowercase()))
        }
        "toUpperCase" => {
            ensure_arity(name, arguments, 1, span)?;
            let input = expect_string(&arguments[0], "toUpperCase", span)?;
            Ok(Value::String(input.to_uppercase()))
        }
        "startsWith" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "startsWith", span)?;
            let prefix = expect_string(&arguments[1], "startsWith", span)?;
            Ok(Value::Bool(input.starts_with(prefix)))
        }
        "endsWith" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "endsWith", span)?;
            let suffix = expect_string(&arguments[1], "endsWith", span)?;
            Ok(Value::Bool(input.ends_with(suffix)))
        }
        "contains" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "contains", span)?;
            let needle = expect_string(&arguments[1], "contains", span)?;
            Ok(Value::Bool(input.contains(needle)))
        }
        "isEmptyString" => {
            ensure_arity(name, arguments, 1, span)?;
            let input = expect_string(&arguments[0], "isEmptyString", span)?;
            Ok(Value::Bool(input.is_empty()))
        }
        "indexOf" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "indexOf", span)?;
            let needle = expect_string(&arguments[1], "indexOf", span)?;
            Ok(Value::Int(
                input.find(needle).map(|index| index as i64).unwrap_or(-1),
            ))
        }
        "lastIndexOf" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "lastIndexOf", span)?;
            let needle = expect_string(&arguments[1], "lastIndexOf", span)?;
            Ok(Value::Int(
                input.rfind(needle).map(|index| index as i64).unwrap_or(-1),
            ))
        }
        "length" => {
            ensure_arity(name, arguments, 1, span)?;
            let input = expect_string(&arguments[0], "length", span)?;
            Ok(Value::Int(input.chars().count() as i64))
        }
        "repeat" => {
            ensure_arity(name, arguments, 2, span)?;
            let input = expect_string(&arguments[0], "repeat", span)?;
            let count = expect_non_negative_int(&arguments[1], "repeat", span)?;
            Ok(Value::String(input.repeat(count)))
        }
        "reverse" => {
            ensure_arity(name, arguments, 1, span)?;
            let input = expect_string(&arguments[0], "reverse", span)?;
            Ok(Value::String(input.chars().rev().collect()))
        }
        "head" => {
            ensure_arity(name, arguments, 1, span)?;
            let list = expect_list(&arguments[0], "head", span)?;
            list.first()
                .cloned()
                .ok_or_else(|| Diagnostic::runtime(span, "head expects a non-empty list"))
        }
        "tail" => {
            ensure_arity(name, arguments, 1, span)?;
            let list = expect_list(&arguments[0], "tail", span)?;
            Ok(Value::List(list.iter().skip(1).cloned().collect()))
        }
        "size" => {
            ensure_arity(name, arguments, 1, span)?;
            let list = expect_list(&arguments[0], "size", span)?;
            Ok(Value::Int(list.len() as i64))
        }
        "isEmpty" => {
            ensure_arity(name, arguments, 1, span)?;
            let list = expect_list(&arguments[0], "isEmpty", span)?;
            Ok(Value::Bool(list.is_empty()))
        }
        "cons" => {
            ensure_arity(name, arguments, 2, span)?;
            let list = expect_list(&arguments[1], "cons", span)?;
            let mut result = Vec::with_capacity(list.len() + 1);
            result.push(arguments[0].clone());
            result.extend(list.iter().cloned());
            Ok(Value::List(result))
        }
        "map" => {
            ensure_arity(name, arguments, 2, span)?;
            let list = expect_list(&arguments[0], "map", span)?;
            let mut result = Vec::with_capacity(list.len());
            for element in list.iter().cloned() {
                result.push(apply_callable(arguments[1].clone(), vec![element], span)?);
            }
            Ok(Value::List(result))
        }
        "foldLeft" => {
            ensure_arity(name, arguments, 3, span)?;
            let list = expect_list(&arguments[0], "foldLeft", span)?;
            let mut acc = arguments[1].clone();
            for element in list.iter().cloned() {
                acc = apply_callable(arguments[2].clone(), vec![acc, element], span)?;
            }
            Ok(acc)
        }
        "FileInput#open" => {
            ensure_arity(name, arguments, 2, span)?;
            let path = expect_string(&arguments[0], "FileInput#open", span)?;
            apply_callable(
                arguments[1].clone(),
                vec![Value::String(path.to_string())],
                span,
            )
        }
        "FileInput#readAll" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "FileInput#readAll", span)?;
            fs::read_to_string(path)
                .map(Value::String)
                .map_err(|error| Diagnostic::runtime(span, format!("failed to read file: {error}")))
        }
        "FileInput#readLines" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "FileInput#readLines", span)?;
            let text = fs::read_to_string(path).map_err(|error| {
                Diagnostic::runtime(span, format!("failed to read file: {error}"))
            })?;
            Ok(Value::List(
                text.lines()
                    .map(|line| Value::String(line.to_string()))
                    .collect(),
            ))
        }
        "FileInput#all" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "FileInput#all", span)?;
            fs::read_to_string(path)
                .map(Value::String)
                .map_err(|error| Diagnostic::runtime(span, format!("failed to read file: {error}")))
        }
        "FileInput#lines" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "FileInput#lines", span)?;
            let text = fs::read_to_string(path).map_err(|error| {
                Diagnostic::runtime(span, format!("failed to read file: {error}"))
            })?;
            Ok(Value::List(
                text.lines()
                    .map(|line| Value::String(line.to_string()))
                    .collect(),
            ))
        }
        "FileOutput#write" => {
            ensure_arity(name, arguments, 2, span)?;
            let path = expect_string(&arguments[0], "FileOutput#write", span)?;
            let content = expect_string(&arguments[1], "FileOutput#write", span)?;
            fs::write(path, content)
                .map(|_| Value::Unit)
                .map_err(|error| {
                    Diagnostic::runtime(span, format!("failed to write file: {error}"))
                })
        }
        "FileOutput#append" => {
            ensure_arity(name, arguments, 2, span)?;
            let path = expect_string(&arguments[0], "FileOutput#append", span)?;
            let content = expect_string(&arguments[1], "FileOutput#append", span)?;
            let mut existing = fs::read_to_string(path).unwrap_or_default();
            existing.push_str(content);
            fs::write(path, existing)
                .map(|_| Value::Unit)
                .map_err(|error| {
                    Diagnostic::runtime(span, format!("failed to append file: {error}"))
                })
        }
        "FileOutput#exists" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "FileOutput#exists", span)?;
            Ok(Value::Bool(std::path::Path::new(path).exists()))
        }
        "FileOutput#delete" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "FileOutput#delete", span)?;
            match fs::remove_file(path) {
                Ok(()) => Ok(Value::Unit),
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(Value::Unit),
                Err(error) => Err(Diagnostic::runtime(
                    span,
                    format!("failed to delete file: {error}"),
                )),
            }
        }
        "FileOutput#writeLines" => {
            ensure_arity(name, arguments, 2, span)?;
            let path = expect_string(&arguments[0], "FileOutput#writeLines", span)?;
            let lines = expect_list(&arguments[1], "FileOutput#writeLines", span)?;
            let text = lines
                .iter()
                .map(|value| match value {
                    Value::String(text) => Ok(text.clone()),
                    _ => Err(Diagnostic::runtime(
                        span,
                        "writeLines expects a list of strings",
                    )),
                })
                .collect::<Result<Vec<_>, _>>()?
                .join("\n");
            fs::write(path, text).map(|_| Value::Unit).map_err(|error| {
                Diagnostic::runtime(span, format!("failed to write lines: {error}"))
            })
        }
        "Dir#current" => std::env::current_dir()
            .map(|path| Value::String(path.display().to_string()))
            .map_err(|error| {
                Diagnostic::runtime(span, format!("failed to get current dir: {error}"))
            }),
        "Dir#home" => env::var("HOME")
            .map(Value::String)
            .map_err(|error| Diagnostic::runtime(span, format!("failed to get home dir: {error}"))),
        "Dir#temp" => Ok(Value::String(env::temp_dir().display().to_string())),
        "Dir#exists" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "Dir#exists", span)?;
            Ok(Value::Bool(std::path::Path::new(path).exists()))
        }
        "Dir#mkdir" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "Dir#mkdir", span)?;
            fs::create_dir(path).map(|_| Value::Unit).map_err(|error| {
                Diagnostic::runtime(span, format!("failed to create dir: {error}"))
            })
        }
        "Dir#mkdirs" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "Dir#mkdirs", span)?;
            fs::create_dir_all(path)
                .map(|_| Value::Unit)
                .map_err(|error| {
                    Diagnostic::runtime(span, format!("failed to create dirs: {error}"))
                })
        }
        "Dir#isDirectory" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "Dir#isDirectory", span)?;
            Ok(Value::Bool(std::path::Path::new(path).is_dir()))
        }
        "Dir#isFile" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "Dir#isFile", span)?;
            Ok(Value::Bool(std::path::Path::new(path).is_file()))
        }
        "Dir#list" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "Dir#list", span)?;
            let mut items = fs::read_dir(path)
                .map_err(|error| Diagnostic::runtime(span, format!("failed to list dir: {error}")))?
                .map(|entry| {
                    entry
                        .map_err(|error| {
                            Diagnostic::runtime(span, format!("failed to read dir entry: {error}"))
                        })
                        .map(|entry| {
                            Value::String(entry.file_name().to_string_lossy().into_owned())
                        })
                })
                .collect::<Result<Vec<_>, _>>()?;
            items.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
            Ok(Value::List(items))
        }
        "Dir#listFull" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "Dir#listFull", span)?;
            let mut items = fs::read_dir(path)
                .map_err(|error| Diagnostic::runtime(span, format!("failed to list dir: {error}")))?
                .map(|entry| {
                    entry
                        .map_err(|error| {
                            Diagnostic::runtime(span, format!("failed to read dir entry: {error}"))
                        })
                        .map(|entry| Value::String(entry.path().display().to_string()))
                })
                .collect::<Result<Vec<_>, _>>()?;
            items.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
            Ok(Value::List(items))
        }
        "Dir#delete" => {
            ensure_arity(name, arguments, 1, span)?;
            let path = expect_string(&arguments[0], "Dir#delete", span)?;
            fs::remove_dir(path).map(|_| Value::Unit).map_err(|error| {
                Diagnostic::runtime(span, format!("failed to delete dir: {error}"))
            })
        }
        "Dir#copy" => {
            ensure_arity(name, arguments, 2, span)?;
            let source = expect_string(&arguments[0], "Dir#copy", span)?;
            let target = expect_string(&arguments[1], "Dir#copy", span)?;
            fs::copy(source, target)
                .map(|_| Value::Unit)
                .map_err(|error| Diagnostic::runtime(span, format!("failed to copy: {error}")))
        }
        "Dir#move" => {
            ensure_arity(name, arguments, 2, span)?;
            let source = expect_string(&arguments[0], "Dir#move", span)?;
            let target = expect_string(&arguments[1], "Dir#move", span)?;
            fs::rename(source, target)
                .map(|_| Value::Unit)
                .map_err(|error| Diagnostic::runtime(span, format!("failed to move: {error}")))
        }
        "Map#containsKey" => {
            ensure_arity(name, arguments, 2, span)?;
            let entries = expect_map(&arguments[0], "Map#containsKey", span)?;
            Ok(Value::Bool(
                entries.iter().any(|(key, _)| key == &arguments[1]),
            ))
        }
        "Map#containsValue" => {
            ensure_arity(name, arguments, 2, span)?;
            let entries = expect_map(&arguments[0], "Map#containsValue", span)?;
            Ok(Value::Bool(
                entries.iter().any(|(_, value)| value == &arguments[1]),
            ))
        }
        "Map#get" => {
            ensure_arity(name, arguments, 2, span)?;
            let entries = expect_map(&arguments[0], "Map#get", span)?;
            Ok(entries
                .iter()
                .find(|(key, _)| key == &arguments[1])
                .map(|(_, value)| value.clone())
                .unwrap_or(Value::Null))
        }
        "Map#isEmpty" => {
            ensure_arity(name, arguments, 1, span)?;
            let entries = expect_map(&arguments[0], "Map#isEmpty", span)?;
            Ok(Value::Bool(entries.is_empty()))
        }
        "Map#size" => {
            ensure_arity(name, arguments, 1, span)?;
            let entries = expect_map(&arguments[0], "Map#size", span)?;
            Ok(Value::Int(entries.len() as i64))
        }
        "Set#contains" => {
            ensure_arity(name, arguments, 2, span)?;
            let values = expect_set(&arguments[0], "Set#contains", span)?;
            Ok(Value::Bool(values.contains(&arguments[1])))
        }
        "Set#isEmpty" => {
            ensure_arity(name, arguments, 1, span)?;
            let values = expect_set(&arguments[0], "Set#isEmpty", span)?;
            Ok(Value::Bool(values.is_empty()))
        }
        "Set#size" => {
            ensure_arity(name, arguments, 1, span)?;
            let values = expect_set(&arguments[0], "Set#size", span)?;
            Ok(Value::Int(values.len() as i64))
        }
        _ => Err(Diagnostic::runtime(
            span,
            format!("`{name}` is not callable in the Rust runtime yet"),
        )),
    }
}

fn prefer_instance_dispatch(name: &str, argument_values: &[Value]) -> bool {
    match (name, argument_values) {
        (
            "map",
            [
                Value::Function(_) | Value::BuiltinFunction(_),
                Value::List(_),
            ],
        ) => true,
        _ => false,
    }
}

fn interpolate_string(
    value: &str,
    span: Span,
    environment: &mut Environment,
) -> Result<Value, Diagnostic> {
    if !value.contains("#{") {
        return Ok(Value::String(value.to_string()));
    }

    let mut result = String::new();
    let chars = value.chars().collect::<Vec<_>>();
    let mut index = 0usize;
    while index < chars.len() {
        if chars[index] == '#' && chars.get(index + 1) == Some(&'{') {
            index += 2;
            let mut expr = String::new();
            let mut paren_depth = 0usize;
            let mut bracket_depth = 0usize;
            while index < chars.len() {
                let ch = chars[index];
                match ch {
                    '{' => expr.push(ch),
                    '(' => {
                        paren_depth += 1;
                        expr.push(ch);
                    }
                    ')' => {
                        paren_depth = paren_depth.saturating_sub(1);
                        expr.push(ch);
                    }
                    '[' => {
                        bracket_depth += 1;
                        expr.push(ch);
                    }
                    ']' => {
                        bracket_depth = bracket_depth.saturating_sub(1);
                        expr.push(ch);
                    }
                    '}' if paren_depth == 0 && bracket_depth == 0 => break,
                    _ => expr.push(ch),
                }
                index += 1;
            }
            if index >= chars.len() || chars[index] != '}' {
                return Err(Diagnostic::runtime(span, "unterminated interpolation"));
            }
            index += 1;
            let normalized = strip_dynamic_cast(expr.trim());
            let parsed = parse_inline_expression("<interpolation>", &normalized).map_err(|_| {
                Diagnostic::runtime(span, "failed to parse interpolation expression")
            })?;
            let mut state = EvaluationState::default();
            let interpolated = eval_expr(&parsed, environment, &mut state)?;
            result.push_str(&interpolated.to_string());
        } else {
            result.push(chars[index]);
            index += 1;
        }
    }

    Ok(Value::String(result))
}

fn strip_dynamic_cast(expression: &str) -> String {
    let trimmed = expression.trim();
    if let Some(prefix) = trimmed.strip_suffix(":> *") {
        prefix.trim_end().to_string()
    } else if let Some(prefix) = trimmed.strip_suffix(":>*") {
        prefix.trim_end().to_string()
    } else {
        trimmed.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::{
        Evaluator, EvaluatorConfig, Value, clear_user_instances, clear_user_modules,
        clear_user_records, evaluate_text, evaluate_text_with_config,
    };
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn evaluates_basic_arithmetic() {
        assert_eq!(evaluate_text("<expr>", "1 + 2 * 3").unwrap(), Value::Int(7));
        assert!(matches!(
            evaluate_text("<expr>", "100L + 2L").unwrap(),
            Value::Long(102)
        ));
        assert!(matches!(
            evaluate_text("<expr>", "2.5F").unwrap(),
            Value::Float(value) if (value - 2.5).abs() < f32::EPSILON
        ));
        assert_eq!(
            evaluate_text("<expr>", "1.5F + 2.0").unwrap(),
            Value::Double(3.5)
        );
        assert_eq!(
            evaluate_text("<expr>", "100S + 2S").unwrap(),
            Value::Int(102)
        );
    }

    #[test]
    fn evaluates_nested_comment_expression() {
        assert_eq!(
            evaluate_text("<expr>", "1 + /* outer /* inner */ outer */ 2").unwrap(),
            Value::Int(3)
        );
    }

    #[test]
    fn evaluates_assignment_and_while() {
        assert_eq!(
            evaluate_text("<expr>", "mutable i = 1\nwhile(i < 4) {\n  i = i + 1\n}\ni",).unwrap(),
            Value::Int(4)
        );
    }

    #[test]
    fn evaluates_if_and_strings() {
        assert_eq!(
            evaluate_text(
                "<expr>",
                "mutable s = \"FOO\"\nif(true) {\n  s = s + s\n}\ns",
            )
            .unwrap(),
            Value::String("FOOFOO".to_string())
        );
    }

    #[test]
    fn evaluates_def_lambda_and_recursion() {
        assert_eq!(
            evaluate_text(
                "<expr>",
                "def add(x, y) = x + y\nval twice = (x) => add(x, x)\ntwice(3)",
            )
            .unwrap(),
            Value::Int(6)
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "def fact(n) = if(n < 2) 1 else (n * fact(n - 1))\nfact(4)",
            )
            .unwrap(),
            Value::Int(24)
        );
        assert_eq!(
            evaluate_text("<expr>", "val add = (x) => (y) => x + y\nadd(1)(2)").unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            evaluate_text("<expr>", "val s: * = (100 :> *)\ns").unwrap(),
            Value::Int(100)
        );
        assert_eq!(
            evaluate_text("<expr>", "val f: ('a) => 'b = null\nf").unwrap(),
            Value::Null
        );
    }

    #[test]
    fn evaluates_ternary_and_builtins() {
        assert_eq!(
            evaluate_text("<expr>", "val x = 1\nx < 2 then \"A\" else \"B\"").unwrap(),
            Value::String("A".to_string())
        );
        assert_eq!(evaluate_text("<expr>", "ceil(2.5)").unwrap(), Value::Int(3));
        assert_eq!(
            evaluate_text("<expr>", "substring(\"FOO\", 1, 3)").unwrap(),
            Value::String("OO".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", "matches(\"199\", \"[0-9]+\")").unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn evaluates_list_literals_and_foreach() {
        assert_eq!(
            evaluate_text("<expr>", "[1 2\n3]").unwrap(),
            Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
        assert_eq!(
            evaluate_text("<expr>", "size([1 2 3])").unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            evaluate_text("<expr>", "cons(2)([1])").unwrap(),
            Value::List(vec![Value::Int(2), Value::Int(1)])
        );
        assert_eq!(
            evaluate_text("<expr>", "map([1 2 3])((x) => x + 1)").unwrap(),
            Value::List(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "mutable total = 0\nforeach(e in [1, 2, 3]) {\n  total += e\n}\ntotal",
            )
            .unwrap(),
            Value::Int(6)
        );
    }

    #[test]
    fn evaluates_string_interpolation_and_string_utils() {
        assert_eq!(
            evaluate_text(
                "<expr>",
                "val x = 10\nval y = 20\n\"x = #{x :> *}, y = #{y :> *}\""
            )
            .unwrap(),
            Value::String("x = 10, y = 20".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", "\"x + y = #{(10 + 20) :> *}\"").unwrap(),
            Value::String("x + y = 30".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", "10.toString()").unwrap(),
            Value::String("10".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", "join(split(\"a b c\", \" \"), \"-\")").unwrap(),
            Value::String("a-b-c".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", "replaceAll(\"123-456-789\", \"[0-9]\", \"X\")").unwrap(),
            Value::String("XXX-XXX-XXX".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", "reverse(trim(\"  hello  \"))").unwrap(),
            Value::String("olleh".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", "stopwatch( => { 1 }) >= 0").unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn evaluates_assertions() {
        assert_eq!(
            evaluate_text("<expr>", "assert(2 == 1 + 1)").unwrap(),
            Value::Unit
        );
        assert_eq!(
            evaluate_text("<expr>", "assertResult(\"A\")(\"A\")").unwrap(),
            Value::Unit
        );
    }

    #[test]
    fn evaluates_thread_builtin() {
        assert_eq!(
            evaluate_text("<expr>", "thread(() => {\n  sleep(1)\n})").unwrap(),
            Value::Unit
        );
    }

    #[test]
    fn shares_mutable_captures_with_threads() {
        assert_eq!(
            evaluate_text(
                "<expr>",
                "mutable counter = 0\nthread(() => {\n  sleep(1)\n  counter = counter + 1\n})\nsleep(10)\ncounter",
            )
            .unwrap(),
            Value::Int(1)
        );
    }

    #[test]
    fn warns_and_denies_trusted_proofs() {
        let program = "trust theorem foo(): { true } = assert(true)";
        let allowed = evaluate_text_with_config(
            "<expr>",
            program,
            EvaluatorConfig {
                deny_trust: false,
                warn_trust: true,
            },
        )
        .expect("trusted theorem should evaluate when only warning");
        assert_eq!(allowed, Value::Unit);

        let error = evaluate_text_with_config(
            "<expr>",
            program,
            EvaluatorConfig {
                deny_trust: true,
                warn_trust: false,
            },
        )
        .expect_err("trusted theorem should be rejected with deny-trust");
        assert!(
            error
                .diagnostic
                .message
                .contains("trusted proof 'foo' is not allowed (level 1)")
        );
    }

    #[test]
    fn deny_trust_reports_first_trusted_proof_in_source_order() {
        let program = "val x = 1\naxiom base(): { true }\ntheorem mid(): { base } = base";
        let error = evaluate_text_with_config(
            "<expr>",
            program,
            EvaluatorConfig {
                deny_trust: true,
                warn_trust: false,
            },
        )
        .expect_err("deny-trust should reject trusted proof chain");
        assert!(
            error
                .diagnostic
                .message
                .contains("trusted proof 'base' is not allowed (level 1)")
        );
    }

    #[test]
    fn evaluates_forward_proof_references() {
        assert_eq!(
            evaluate_text(
                "<expr>",
                "theorem earlier(x: Int): { later(x) } = assert(true)\naxiom later(y: Int): { true }\nearlier(1)",
            )
            .unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn evaluates_higher_kinded_constraints_via_bound_dictionaries() {
        let program = "typeclass Monad<'m: * => *> where {\n  bind: ('m<'a>, ('a) => 'm<'b>) => 'm<'b>;\n  unit: ('a) => 'm<'a>\n}\ninstance Monad<List> where {\n  def bind(xs: List<'a>, f: ('a) => List<'b>): List<'b> = f(head(xs))\n  def unit(x: 'a): List<'a> = [x]\n}\ndef pairWithNext<'m>(xs: 'm<Int>): 'm<Int> where Monad<'m> = bind(xs, (x) => unit(x + 1))\npairWithNext([1, 2, 3])";
        assert_eq!(
            evaluate_text("<expr>", program).unwrap(),
            Value::List(vec![Value::Int(2)])
        );
    }

    #[test]
    fn native_calls_do_not_double_evaluate_arguments() {
        assert_eq!(
            evaluate_text(
                "<expr>",
                "mutable counter = 0\nval incrementAndReturn = () => {\n  counter = counter + 1\n  counter\n}\nprintln(incrementAndReturn())\ncounter",
            )
            .unwrap(),
            Value::Int(1)
        );
    }

    #[test]
    fn rejects_partial_application_for_non_curried_user_functions() {
        let error = evaluate_text("<expr>", "def f(x, y) = x + y\nf(10)")
            .expect_err("non-curried partial application should fail");
        assert!(
            error
                .to_string()
                .contains("function expects 2 arguments but got 1")
        );
    }

    #[test]
    fn evaluates_file_and_dir_modules() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("klassic-rust-{unique}"));
        let file = dir.join("sample.txt");
        let moved = dir.join("sample-moved.txt");
        let empty_child = dir.join("empty-child");

        evaluate_text(
            "<expr>",
            &format!(
                "Dir#mkdir(\"{}\")\nFileOutput#write(\"{}\", \"hello\")",
                dir.display(),
                file.display()
            ),
        )
        .unwrap();

        assert_eq!(
            evaluate_text("<expr>", &format!("FileInput#all(\"{}\")", file.display())).unwrap(),
            Value::String("hello".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                &format!(
                    "\"{}\" FileInput#open {{ stream => FileInput#readAll(stream) }}",
                    file.display()
                ),
            )
            .unwrap(),
            Value::String("hello".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", &format!("Dir#isFile(\"{}\")", file.display())).unwrap(),
            Value::Bool(true)
        );
        let listed =
            evaluate_text("<expr>", &format!("Dir#listFull(\"{}\")", dir.display())).unwrap();
        let Value::List(listed) = listed else {
            panic!("Dir#listFull should return a list");
        };
        assert!(
            listed
                .iter()
                .any(|value| matches!(value, Value::String(path) if path.ends_with("sample.txt")))
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                &format!("Dir#copy(\"{}\", \"{}\")", file.display(), moved.display())
            )
            .unwrap(),
            Value::Unit
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                &format!("FileOutput#exists(\"{}\")", moved.display())
            )
            .unwrap(),
            Value::Bool(true)
        );
        fs::create_dir(&empty_child).unwrap();
        assert_eq!(
            evaluate_text(
                "<expr>",
                &format!("Dir#delete(\"{}\")", empty_child.display())
            )
            .unwrap(),
            Value::Unit
        );
        assert_eq!(
            evaluate_text("<expr>", &format!("Dir#delete(\"{}\")", dir.display())).is_err(),
            true
        );

        let _ = fs::remove_file(&file);
        let _ = fs::remove_file(&moved);
        let _ = fs::remove_dir(&dir);
    }

    #[test]
    fn evaluates_file_output_append_and_exists() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let file = std::env::temp_dir().join(format!("klassic-file-output-{unique}.txt"));

        evaluate_text(
            "<expr>",
            &format!(
                "FileOutput#write(\"{}\", \"Line 1\")\nFileOutput#append(\"{}\", \"\\nLine 2\")",
                file.display(),
                file.display()
            ),
        )
        .unwrap();

        assert_eq!(
            evaluate_text(
                "<expr>",
                &format!("FileOutput#exists(\"{}\")", file.display())
            )
            .unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_text("<expr>", &format!("FileInput#all(\"{}\")", file.display())).unwrap(),
            Value::String("Line 1\nLine 2".to_string())
        );

        let _ = fs::remove_file(file);
    }

    #[test]
    fn evaluates_file_input_open_and_lines() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let file = std::env::temp_dir().join(format!("klassic-file-input-{unique}.txt"));
        fs::write(&file, "Hello, World!").unwrap();

        assert_eq!(
            evaluate_text(
                "<expr>",
                &format!(
                    "\"{}\" FileInput#open {{ stream => FileInput#readAll(stream) }}",
                    file.display()
                ),
            )
            .unwrap(),
            Value::String("Hello, World!".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                &format!(
                    "\"{}\" FileInput#open {{ stream => FileInput#readLines(stream) }}",
                    file.display()
                ),
            )
            .unwrap(),
            Value::List(vec![Value::String("Hello, World!".to_string())])
        );

        let _ = fs::remove_file(file);
    }

    #[test]
    fn evaluates_map_and_set_literals_and_modules() {
        assert_eq!(
            evaluate_text("<expr>", "%[\"a\": 1 \"b\": 2]").unwrap(),
            Value::Map(vec![
                (Value::String("a".to_string()), Value::Int(1)),
                (Value::String("b".to_string()), Value::Int(2)),
            ])
        );
        assert_eq!(
            evaluate_text("<expr>", "%(1 2 3)").unwrap(),
            Value::Set(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
        assert_eq!(
            evaluate_text("<expr>", "Map#containsKey(%[\"x\": 1 \"y\": 2], \"x\")").unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_text("<expr>", "Map#get(%[\"x\": 1], \"z\")").unwrap(),
            Value::Null
        );
        assert_eq!(
            evaluate_text("<expr>", "Set#contains(%(1, 2, 3), 2)").unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_text("<expr>", "%[\"x\": 1 \"y\": 2] Map#get \"y\"").unwrap(),
            Value::Int(2)
        );
    }

    #[test]
    fn evaluates_module_headers_and_imports() {
        clear_user_modules();

        assert_eq!(
            evaluate_text(
                "<expr>",
                "module foo.bar\nimport Map\nsize(%[\"A\": 1, \"B\": 2])",
            )
            .unwrap(),
            Value::Int(2)
        );
        assert_eq!(
            evaluate_text("<expr>", "import Set\ncontains(%(1, 2, 3))(2)").unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "import Map as M\nimport Map.{size}\nM#size(%[\"A\": 1]) + size(%[\"B\": 2])",
            )
            .unwrap(),
            Value::Int(2)
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "import Map.{size, get => _}\nsize(%[\"A\": 1, \"B\": 2])",
            )
            .unwrap(),
            Value::Int(2)
        );
    }

    #[test]
    fn evaluates_user_defined_modules() {
        clear_user_modules();

        evaluate_text("<expr>", "module m\ndef twice(x) = x + x").unwrap();
        assert_eq!(
            evaluate_text("<expr>", "import m\ntwice(21)").unwrap(),
            Value::Int(42)
        );

        evaluate_text("<expr>", "module user.util\ndef inc(x) = x + 1").unwrap();
        assert_eq!(
            evaluate_text("<expr>", "import user.util\ninc(41)").unwrap(),
            Value::Int(42)
        );
        evaluate_text(
            "<expr>",
            "module hidden.test\ndef allowed() = 1\ndef hidden() = 2",
        )
        .unwrap();
        assert_eq!(
            evaluate_text(
                "<expr>",
                "import hidden.test.{allowed, hidden => _}\nallowed()",
            )
            .unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            evaluate_text("<expr>", "user.util#inc(41)").unwrap(),
            Value::Int(42)
        );
    }

    #[test]
    fn evaluates_record_declarations_and_access() {
        clear_user_records();

        assert_eq!(
            evaluate_text(
                "<expr>",
                "record Person {\n  name: *\n  age: Int\n}\n#Person(\"Hoge\", 7)",
            )
            .unwrap(),
            Value::Record {
                name: "Person".to_string(),
                fields: vec![
                    ("name".to_string(), Value::String("Hoge".to_string())),
                    ("age".to_string(), Value::Int(7)),
                ],
            }
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "record Tuple<'a, 'b> {\n  _1: 'a\n  _2: 'b\n}\nval t = #Tuple(1, 2)\nt._1",
            )
            .unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            evaluate_text("<expr>", "val point = #Point(1000, 2000)\npoint.y").unwrap(),
            Value::Int(2000)
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "record S {\n  v: Int\n  to_string: (*) => *\n}\nval s = #S(100, (this) => { \"#{(this :> *).v}\" })\ns.to_string()",
            )
            .unwrap(),
            Value::String("100".to_string())
        );
        assert_eq!(
            evaluate_text("<expr>", "rule {\n  S = \"a\";\n}\n1").unwrap(),
            Value::Int(1)
        );
    }

    #[test]
    fn evaluates_typeclass_dispatch() {
        clear_user_instances();
        clear_user_records();

        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Show<String> where {\n  def show(x: String): String = \"String(\" + x + \")\"\n}\nshow(42)",
            )
            .unwrap(),
            Value::String("Int(42)".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Eq<'a> where {\n  equals: ('a, 'a) => Boolean\n}\nrecord Person {\n  name: String\n  age: Int\n}\ninstance Eq<Int> where {\n  def equals(x: Int, y: Int): Boolean = x == y\n}\ninstance Eq<String> where {\n  def equals(x: String, y: String): Boolean = x == y\n}\ninstance Eq<Person> where {\n  def equals(p1: Person, p2: Person): Boolean = equals(p1.name, p2.name) && equals(p1.age, p2.age)\n}\nval p1 = #Person(\"Alice\", 30)\nval p2 = #Person(\"Alice\", 30)\nequals(p1, p2)",
            )
            .unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Functor<'f: * => *> where {\n  map: (('a) => 'b, 'f<'a>) => 'f<'b>\n}\ninstance Functor<List> where {\n  def map(f: ('a) => 'b, xs: List<'a>): List<'b> = {\n    xs.map(f)\n  }\n}\nmap((x) => x * 2, [1, 2, 3])",
            )
            .unwrap(),
            Value::List(vec![Value::Int(2), Value::Int(4), Value::Int(6)])
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Serializable<'a> where {\n  serialize: ('a) => String\n}\ninstance Serializable<Int> where {\n  def serialize(x: Int): String = x.toString()\n}\ninstance Serializable<List<Int>> where {\n  def serialize(xs: List<Int>): String = \"[\" + join(map(xs)((x) => serialize(x)), \",\") + \"]\"\n}\nserialize([10, 20, 30])",
            )
            .unwrap(),
            Value::String("[10,20,30]".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Eq<'a> where {\n  equals: ('a, 'a) => Boolean\n  notEquals: ('a, 'a) => Boolean\n}\ninstance Eq<Int> where {\n  def equals(x: Int, y: Int): Boolean = x == y\n  def notEquals(x: Int, y: Int): Boolean = x != y\n}\nnotEquals(10, 20)",
            )
            .unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int: \" + x\n}\ninstance Show<String> where {\n  def show(x: String): String = \"Str: \" + x\n}\nval result1 = show(42)\nval result2 = show(\"hello\")\nassert(result1 == \"Int: 42\")\nassert(result2 == \"Str: hello\")\n\"Type class methods work\"",
            )
            .unwrap(),
            Value::String("Type class methods work".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Functor<'f: * => *> where {\n  map: (('a) => 'b, 'f<'a>) => 'f<'b>\n}\ninstance Functor<List> where {\n  def map(f: ('a) => 'b, xs: List<'a>): List<'b> = {\n    xs.map(f)\n  }\n}\nval numbers = [1, 2, 3]\nmap((x) => x * 2, numbers)",
            )
            .unwrap(),
            Value::List(vec![Value::Int(2), Value::Int(4), Value::Int(6)])
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Display<'a> where {\n  display: ('a) => String\n}\nrecord Point {\n  x: Int\n  y: Int\n}\ninstance Display<Point> where {\n  def display(p: Point): String = \"(\" + p.x + \",\" + p.y + \")\"\n}\nval p = #Point(3, 4)\ndisplay(p)",
            )
            .unwrap(),
            Value::String("(3,4)".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Printable<'a> where {\n  print: ('a) => String\n}\ninstance Printable<Int> where {\n  def print(x: Int): String = \"Int=\" + x\n}\ninstance Printable<Boolean> where {\n  def print(x: Boolean): String = if(x) \"YES\" else \"NO\"\n}\nval s1 = print(42)\nval s2 = print(true)\nval s3 = print(false)\ns1 + \", \" + s2 + \", \" + s3",
            )
            .unwrap(),
            Value::String("Int=42, YES, NO".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Show<'a> where {\n  show: ('a) => String\n}\ntypeclass Eq<'a> where {\n  eq: ('a, 'a) => Boolean\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Eq<Int> where {\n  def eq(x: Int, y: Int): Boolean = x == y\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\ndef show_if_equal<'a>(x: 'a, y: 'a): String where (Show<'a>, Eq<'a>) = if(eq(x, y)) show(x) else show(y)\ndisplay(42) + \",\" + show_if_equal(1, 2)",
            )
            .unwrap(),
            Value::String("Int(42),Int(2)".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Show<List<'a>> where Show<'a> {\n  def show(xs: List<'a>): String = \"[\" + join(map(xs)((x) => show(x)), \",\") + \"]\"\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\ndisplay([1, 2, 3])",
            )
            .unwrap(),
            Value::String("[Int(1),Int(2),Int(3)]".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Show<List<'a>> where Show<'a> {\n  def show(xs: List<'a>): String = \"[\" + join(map(xs)((x) => show(x)), \",\") + \"]\"\n}\ndef show_list<'a>(xs: List<'a>): String where Show<'a> = show(xs)\nshow_list([1, 2, 3])",
            )
            .unwrap(),
            Value::String("[Int(1),Int(2),Int(3)]".to_string())
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Functor<'f: * => *> where {\n  map: (('a) => 'b, 'f<'a>) => 'f<'b>\n}\ninstance Functor<List> where {\n  def map(f: ('a) => 'b, xs: List<'a>): List<'b> = xs.map(f)\n}\ndef liftTwice<'f, 'a, 'b, 'c>(xs: 'f<'a>, f: ('a) => 'b, g: ('b) => 'c): 'f<'c> where Functor<'f> = map(g, map(f, xs))\nliftTwice([1, 2, 3], (x) => x + 1, (y) => y * 2)",
            )
            .unwrap(),
            Value::List(vec![Value::Int(4), Value::Int(6), Value::Int(8)])
        );
        assert_eq!(
            evaluate_text(
                "<expr>",
                "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Show<String> where {\n  def show(x: String): String = \"Str(\" + x + \")\"\n}\nrecord Person {\n  name: String\n  age: Int\n}\ninstance Show<Person> where {\n  def show(p: Person): String = \"Person(\" + p.name + \")\"\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\ndisplay(42) + \",\" + display(\"hello\") + \",\" + display(#Person(\"Alice\", 30))",
            )
            .unwrap(),
            Value::String("Int(42),Str(hello),Person(Alice)".to_string())
        );
    }

    #[test]
    fn evaluates_placeholder_desugaring() {
        clear_user_modules();
        clear_user_instances();

        assert_eq!(
            evaluate_text("<expr>", "val xs = [1 2 3]\nmap(xs)(_ + 1)").unwrap(),
            Value::List(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
        );
        assert_eq!(
            evaluate_text("<expr>", "foldLeft([1 2 3])(0)(_ + _)").unwrap(),
            Value::Int(6)
        );
        assert_eq!(
            evaluate_text("<expr>", "val id = _\nmap([1])(id)").unwrap(),
            Value::List(vec![Value::Int(1)])
        );
        assert_eq!(
            evaluate_text("<expr>", "def f(x) = _\nmap([1])(f(1))").unwrap(),
            Value::List(vec![Value::Int(1)])
        );
        assert_eq!(
            evaluate_text("<expr>", "val xs = [1 2 3]\nxs.map(_ + 1)").unwrap(),
            Value::List(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
        );
        assert_eq!(
            evaluate_text("<expr>", "map([2 3 4]){x => x + 1}").unwrap(),
            Value::List(vec![Value::Int(3), Value::Int(4), Value::Int(5)])
        );
        assert_eq!(
            evaluate_text("<expr>", "foldLeft([1.0 2.0 3.0 4.0])(1.0){x, y => x * y}").unwrap(),
            Value::Double(24.0)
        );
        assert_eq!(
            evaluate_text("<expr>", "[1 2 3] map x => x + 1").unwrap(),
            Value::List(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
        );
        assert_eq!(
            evaluate_text("<expr>", "[1 2 3 4] reduce 0 => r + e").unwrap(),
            Value::Int(10)
        );
    }

    #[test]
    fn evaluates_cleanup_expressions() {
        clear_user_modules();

        assert_eq!(
            evaluate_text(
                "<expr>",
                "mutable i = 0\nwhile(i < 3) {\n  i += 1\n} cleanup {\n  i += 10\n}\ni",
            )
            .unwrap(),
            Value::Int(13)
        );
        assert_eq!(
            evaluate_text("<expr>", "def none() = 24 cleanup \"none\"\nnone()").unwrap(),
            Value::Int(24)
        );
    }

    #[test]
    fn evaluator_persists_repl_like_state() {
        clear_user_modules();

        let mut evaluator = Evaluator::new();
        assert_eq!(
            evaluator
                .evaluate_text("<repl>", "val answer = 42")
                .unwrap(),
            Value::Unit
        );
        assert_eq!(
            evaluator.evaluate_text("<repl>", "answer").unwrap(),
            Value::Int(42)
        );
        assert_eq!(
            evaluator
                .evaluate_text("<repl>", "val next: Int = answer + 1\nnext")
                .unwrap(),
            Value::Int(43)
        );
        let error = evaluator
            .evaluate_text("<repl>", "val nope: String = answer")
            .expect_err("persisted binding type should be reused");
        assert!(error.to_string().contains("type mismatch"));
    }

    #[test]
    fn evaluator_persists_generic_record_types_across_repl_turns() {
        clear_user_records();

        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_text(
                "<repl>",
                "record Pair<'a, 'b> {\n  _1: 'a\n  _2: 'b\n}\nval pair = #Pair(1, 2.0)",
            )
            .unwrap();
        assert_eq!(
            evaluator
                .evaluate_text("<repl>", "val left: Int = pair._1\nleft")
                .unwrap(),
            Value::Int(1)
        );
        let error = evaluator
            .evaluate_text("<repl>", "val nope: String = pair._1")
            .expect_err("persisted generic record field type should be reused");
        assert!(error.to_string().contains("type mismatch"));
    }

    #[test]
    fn evaluator_persists_polymorphic_bindings_across_repl_turns() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_text("<repl>", "val id = (x) => x")
            .expect("id should define");
        assert_eq!(
            evaluator
                .evaluate_text("<repl>", "val a: Int = id(1)\na")
                .unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            evaluator
                .evaluate_text("<repl>", "val b: String = id(\"ok\")\nb")
                .unwrap(),
            Value::String("ok".to_string())
        );
    }

    #[test]
    fn evaluates_structural_record_dictionary_calls() {
        clear_user_records();

        assert_eq!(
            evaluate_text(
                "<expr>",
                "val show_int = record { show: (x: Int) => \"Int(\" + x + \")\" }\ndef show_value(dict: record { show: (Int) => String }, x: Int): String = dict.show(x)\nshow_value(show_int, 42)",
            )
            .unwrap(),
            Value::String("Int(42)".to_string())
        );
    }

    #[test]
    fn evaluates_row_polymorphic_field_access() {
        clear_user_records();

        assert_eq!(
            evaluate_text(
                "<expr>",
                "record P {\n  x: Int\n  y: Int\n  z: Int\n}\nrecord T<'a, 'b> {\n  x: 'a\n  y: 'b\n}\ndef add_xy(o) = {\n  o.x + o.y\n}\nadd_xy(#P(1, 2, 3)) + add_xy(#T(3, 4))",
            )
            .unwrap(),
            Value::Int(10)
        );
    }

    #[test]
    fn rejects_assignment_to_immutable_bindings() {
        let error =
            evaluate_text("<expr>", "val a = 1\na = 2").expect_err("assignment should fail");
        assert!(
            error
                .to_string()
                .contains("cannot assign to immutable binding")
        );
    }

    #[test]
    fn reports_runtime_errors() {
        let error = evaluate_text("<expr>", "1 / 0").expect_err("division by zero should fail");
        assert!(error.to_string().contains("division by zero"));

        let todo = evaluate_text(
            "<expr>",
            "def fact(n) = if(n < 2) ToDo() else n * fact(n - 1)\nfact(0)",
        )
        .expect_err("ToDo should fail");
        assert!(todo.to_string().contains("not implemented yet"));
    }
}
