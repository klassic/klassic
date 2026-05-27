//! Shared proof / trust analysis used by `klassic-eval` and `klassic-native`.
//!
//! The analysis itself (collecting `theorem` and `axiom` declarations,
//! computing transitive trust levels, and either warning or denying when
//! trusted proofs appear) is identical for both consumers. The only thing
//! that differs is the kind of [`Diagnostic`] each caller wants to emit on
//! failure — evaluator-side errors are `DiagnosticKind::Type`, native-side
//! errors are `DiagnosticKind::Compile`. The caller passes the constructor
//! it needs through [`ProofConfig::build_error`] so the rendered stderr
//! output for `--warn-trust` and `--deny-trust` stays exactly the same as
//! before this code was unified.

use std::collections::{HashMap, HashSet};

use klassic_span::{Diagnostic, Span};
use klassic_syntax::Expr;

#[derive(Clone, Debug)]
pub struct ProofDefinition {
    pub name: String,
    pub span: Span,
    pub proposition: Expr,
    pub body: Option<Expr>,
    pub trusted: bool,
    pub is_axiom: bool,
}

#[derive(Clone, Debug)]
pub struct ProofMetadata {
    pub name: String,
    pub span: Span,
    pub level: usize,
    pub trusted: bool,
    pub dependencies: HashSet<String>,
}

/// Flags and error constructor that drive [`analyze_proofs`].
///
/// `build_error` lets the caller choose the [`Diagnostic`] kind it expects
/// (`DiagnosticKind::Type` for the evaluator, `DiagnosticKind::Compile`
/// for the native compiler). The function is shared between the
/// `--deny-trust` failure path and the cyclic-dependency error path, so
/// both errors land with consistent kinds for each consumer.
pub struct ProofConfig<'f> {
    pub deny_trust: bool,
    pub warn_trust: bool,
    pub build_error: &'f dyn Fn(Span, String) -> Diagnostic,
}

pub fn analyze_proofs(expr: &Expr, config: ProofConfig<'_>) -> Result<(), Diagnostic> {
    let definitions = collect_proof_definitions(expr);
    if definitions.is_empty() {
        return Ok(());
    }
    let metadata = compute_proof_metadata(&definitions, config.build_error)?;
    if config.deny_trust
        && let Some(proof) = definitions
            .iter()
            .filter_map(|definition| metadata.get(&definition.name))
            .find(|proof| proof.trusted)
    {
        return Err((config.build_error)(
            proof.span,
            format!(
                "trusted proof '{}' is not allowed (level {})",
                proof.name, proof.level
            ),
        ));
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

pub fn collect_proof_definitions(expr: &Expr) -> Vec<ProofDefinition> {
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

pub fn compute_proof_metadata(
    definitions: &[ProofDefinition],
    build_error: &dyn Fn(Span, String) -> Diagnostic,
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
        build_error: &dyn Fn(Span, String) -> Diagnostic,
    ) -> Result<usize, Diagnostic> {
        if let Some(level) = memo.get(name) {
            return Ok(*level);
        }
        if !visiting.insert(name.to_string()) {
            let span = by_name
                .get(name)
                .map(|definition| definition.span)
                .unwrap_or(Span::new(0, 0));
            return Err(build_error(
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
                let dep_level =
                    compute_level(dependency, by_name, deps, memo, visiting, build_error)?;
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
        compute_level(
            &definition.name,
            &by_name,
            &deps,
            &mut memo,
            &mut visiting,
            build_error,
        )?;
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

pub fn proof_dependencies(
    definition: &ProofDefinition,
    names: &HashSet<String>,
) -> HashSet<String> {
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
        | Expr::TypeClassDeclaration { .. }
        | Expr::PegRuleBlock { .. }
        | Expr::Int { .. }
        | Expr::Double { .. }
        | Expr::Bool { .. }
        | Expr::String { .. }
        | Expr::Null { .. }
        | Expr::Unit { .. } => {}
        Expr::InstanceDeclaration { methods, .. }
        | Expr::ExtensionDeclaration { methods, .. }
        | Expr::Block {
            expressions: methods,
            ..
        } => {
            for method in methods {
                collect_referenced_proof_names(method, names, dependencies);
            }
        }
        Expr::EnumDeclaration { .. } => {}
        Expr::Match {
            scrutinee, arms, ..
        } => {
            collect_referenced_proof_names(scrutinee, names, dependencies);
            for arm in arms {
                collect_referenced_proof_names(&arm.body, names, dependencies);
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
        Expr::VarDecl { value, .. } | Expr::Assign { value, .. } => {
            collect_referenced_proof_names(value, names, dependencies);
        }
        Expr::Unary { expr: value, .. } => {
            collect_referenced_proof_names(value, names, dependencies);
        }
        Expr::DefDecl { body, .. } | Expr::Lambda { body, .. } => {
            collect_referenced_proof_names(body, names, dependencies);
        }
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
        Expr::Cleanup { body, cleanup, .. } => {
            collect_referenced_proof_names(body, names, dependencies);
            collect_referenced_proof_names(cleanup, names, dependencies);
        }
        Expr::While {
            condition, body, ..
        } => {
            collect_referenced_proof_names(condition, names, dependencies);
            collect_referenced_proof_names(body, names, dependencies);
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
        Expr::RecordLiteral { fields, .. } => {
            for (_, value) in fields {
                collect_referenced_proof_names(value, names, dependencies);
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
