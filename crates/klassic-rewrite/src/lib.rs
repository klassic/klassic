use klassic_syntax::{Expr, StringPart};

pub fn rewrite_expression(expr: Expr) -> Expr {
    finalize(expr)
}

fn finalize(expr: Expr) -> Expr {
    let span = expr.span();
    wrap_placeholders(rewrite(expr), span)
}

// A flat, left-leaning operator chain (`1+1+1+...+1`) walks `rewrite`
// recursively one AST level per operator, with no trampoline or tail-call
// elimination in sight (`rewrite`'s `Expr::Binary` arm calls `rewrite(*lhs)`
// before it can finish building the node). A chain of a few thousand terms
// is ordinary user code and must not be rejected, but left unguarded it
// blows the host stack — a 2 MiB debug test-thread stack overflowed at
// roughly 100-150 terms once the O(n^2) redundant placeholder walk fixed
// below was no longer masking the pure recursion-depth cost (see
// `replace_placeholders`'s doc comment). `stacker::maybe_grow` adds a
// fresh stack segment whenever the red zone is reached, mirroring the
// guard `klassic-eval::eval_expr` already uses for the same shape of
// problem (unbounded user recursion with 1:1 host frames). The knobs here
// reuse `klassic-eval`'s sizing: 512 KiB proved comfortably larger than a
// single `rewrite` frame (Expr's many Box<Expr> fields plus the surrounding
// match arm are on the order of a few hundred bytes to low KiBs per frame
// in debug builds) so the guard fires well before the frame that triggered
// it could itself exhaust the zone, and 8 MiB growth keeps the number of
// segment allocations low for very long chains (100k+ terms).
const STACK_RED_ZONE: usize = 512 * 1024;
const STACK_GROW_SIZE: usize = 8 * 1024 * 1024;

fn rewrite(expr: Expr) -> Expr {
    stacker::maybe_grow(STACK_RED_ZONE, STACK_GROW_SIZE, || rewrite_inner(expr))
}

fn rewrite_inner(expr: Expr) -> Expr {
    match expr {
        Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span,
        } => Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span,
        },
        Expr::RecordLiteral { fields, span } => wrap_placeholders(
            Expr::RecordLiteral {
                fields: fields
                    .into_iter()
                    .map(|(name, value)| (name, rewrite(value)))
                    .collect(),
                span,
            },
            span,
        ),
        Expr::Null { span } => Expr::Null { span },
        Expr::TypeClassDeclaration {
            name,
            type_params,
            methods,
            span,
        } => Expr::TypeClassDeclaration {
            name,
            type_params,
            methods,
            span,
        },
        Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation,
            constraints,
            methods,
            span,
        } => Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation,
            constraints,
            methods: methods.into_iter().map(finalize).collect(),
            span,
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
            name,
            params,
            param_annotations,
            proposition: Box::new(finalize(*proposition)),
            body: Box::new(finalize(*body)),
            trusted,
            span,
        },
        Expr::AxiomDeclaration {
            name,
            params,
            param_annotations,
            proposition,
            span,
        } => Expr::AxiomDeclaration {
            name,
            params,
            param_annotations,
            proposition: Box::new(finalize(*proposition)),
            span,
        },
        Expr::PegRuleBlock { span } => Expr::PegRuleBlock { span },
        Expr::VarDecl {
            mutable,
            name,
            annotation,
            value,
            span,
        } => Expr::VarDecl {
            mutable,
            name,
            annotation,
            value: Box::new(finalize(*value)),
            span,
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
            name,
            type_params,
            constraints,
            params,
            param_annotations,
            return_annotation,
            body: Box::new(finalize(*body)),
            span,
        },
        Expr::Lambda {
            params,
            param_annotations,
            body,
            span,
        } => Expr::Lambda {
            params,
            param_annotations,
            body: Box::new(finalize(*body)),
            span,
        },
        Expr::Assign { name, value, span } => Expr::Assign {
            name,
            value: Box::new(finalize(*value)),
            span,
        },
        Expr::Unary { op, expr, span } => wrap_placeholders(
            Expr::Unary {
                op,
                expr: Box::new(rewrite(*expr)),
                span,
            },
            span,
        ),
        Expr::Binary { lhs, op, rhs, span } => wrap_placeholders(
            Expr::Binary {
                lhs: Box::new(rewrite(*lhs)),
                op,
                rhs: Box::new(rewrite(*rhs)),
                span,
            },
            span,
        ),
        Expr::Call {
            callee,
            arguments,
            span,
        } => wrap_placeholders(
            Expr::Call {
                callee: Box::new(rewrite(*callee)),
                arguments: arguments.into_iter().map(rewrite).collect(),
                span,
            },
            span,
        ),
        Expr::FieldAccess {
            target,
            field,
            span,
        } => wrap_placeholders(
            Expr::FieldAccess {
                target: Box::new(rewrite(*target)),
                field,
                span,
            },
            span,
        ),
        Expr::Cleanup {
            body,
            cleanup,
            span,
        } => wrap_placeholders(
            Expr::Cleanup {
                body: Box::new(rewrite(*body)),
                cleanup: Box::new(rewrite(*cleanup)),
                span,
            },
            span,
        ),
        Expr::RecordConstructor {
            name,
            arguments,
            span,
        } => wrap_placeholders(
            Expr::RecordConstructor {
                name,
                arguments: arguments.into_iter().map(rewrite).collect(),
                span,
            },
            span,
        ),
        Expr::ListLiteral { elements, span } => wrap_placeholders(
            Expr::ListLiteral {
                elements: elements.into_iter().map(rewrite).collect(),
                span,
            },
            span,
        ),
        Expr::MapLiteral { entries, span } => wrap_placeholders(
            Expr::MapLiteral {
                entries: entries
                    .into_iter()
                    .map(|(key, value)| (rewrite(key), rewrite(value)))
                    .collect(),
                span,
            },
            span,
        ),
        Expr::SetLiteral { elements, span } => wrap_placeholders(
            Expr::SetLiteral {
                elements: elements.into_iter().map(rewrite).collect(),
                span,
            },
            span,
        ),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => wrap_placeholders(
            Expr::If {
                condition: Box::new(rewrite(*condition)),
                then_branch: Box::new(rewrite(*then_branch)),
                else_branch: else_branch.map(|branch| Box::new(rewrite(*branch))),
                span,
            },
            span,
        ),
        Expr::While {
            condition,
            body,
            span,
        } => wrap_placeholders(
            Expr::While {
                condition: Box::new(rewrite(*condition)),
                body: Box::new(rewrite(*body)),
                span,
            },
            span,
        ),
        Expr::Foreach {
            binding,
            iterable,
            body,
            span,
        } => wrap_placeholders(
            Expr::Foreach {
                binding,
                iterable: Box::new(rewrite(*iterable)),
                body: Box::new(rewrite(*body)),
                span,
            },
            span,
        ),
        Expr::Block { expressions, span } => Expr::Block {
            expressions: expressions.into_iter().map(finalize).collect(),
            span,
        },
        Expr::StringInterpolation { parts, span } => wrap_placeholders(
            Expr::StringInterpolation {
                parts: parts
                    .into_iter()
                    .map(|part| match part {
                        StringPart::Literal(text) => StringPart::Literal(text),
                        StringPart::Interpolation(hole) => {
                            StringPart::Interpolation(Box::new(rewrite(*hole)))
                        }
                    })
                    .collect(),
                span,
            },
            span,
        ),
        other => other,
    }
}

fn wrap_placeholders(expr: Expr, span: klassic_span::Span) -> Expr {
    let mut counter = 0usize;
    let rewritten = replace_placeholders(expr, &mut counter);
    if counter == 0 {
        rewritten
    } else {
        Expr::Lambda {
            params: (0..counter)
                .map(|index| format!("__placeholder{index}"))
                .collect(),
            param_annotations: (0..counter).map(|_| None).collect(),
            body: Box::new(rewritten),
            span,
        }
    }
}

// `replace_placeholders` looks for a bare `_` sitting directly in one of
// `expr`'s own fields (e.g. the `lhs`/`rhs` of a `Binary`, or an argument
// of a `Call`) so it can turn the innermost enclosing wrap-eligible node
// into a lambda. It deliberately does NOT recurse past that one level: every
// nested sub-expression that could itself hold a placeholder is a node kind
// `rewrite` already drove through its own `wrap_placeholders` call (or
// through `finalize`, for lambda/def/block-style scopes) while building
// `expr`, so by the time `expr` reaches here every reachable placeholder
// below the immediate-child level has already been consumed and converted
// into a `Lambda`. A deep re-walk here would just re-visit that
// already-placeholder-free subtree.
//
// This matters beyond tidiness: a flat, left-leaning chain like
// `1+1+1+...+1` used to call the old deep `replace_placeholders` once per
// `+` as `rewrite` unwound its own recursion, each call re-scanning the
// entire (already-processed) left spine built so far. That made the whole
// pass O(n^2) in the chain length and, combined with the plain recursion,
// blew the native call stack long before quadratic time became the
// dominant problem (a 5,000-term chain overflowed a 2 MiB test-thread
// stack in debug builds). Restricting the search to direct children keeps
// this pass linear in the size of the AST and bounds its own recursion
// depth to one field-access frame, regardless of chain length.
fn replace_placeholders(expr: Expr, counter: &mut usize) -> Expr {
    match expr {
        Expr::Identifier { name, span } if name == "_" => {
            let name = format!("__placeholder{}", *counter);
            *counter += 1;
            Expr::Identifier { name, span }
        }
        Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span,
        } => Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span,
        },
        Expr::TypeClassDeclaration {
            name,
            type_params,
            methods,
            span,
        } => Expr::TypeClassDeclaration {
            name,
            type_params,
            methods,
            span,
        },
        Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation,
            constraints,
            methods,
            span,
        } => Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation,
            constraints,
            methods: methods
                .into_iter()
                .map(|method| replace_direct_child(method, counter))
                .collect(),
            span,
        },
        Expr::PegRuleBlock { span } => Expr::PegRuleBlock { span },
        Expr::VarDecl {
            mutable,
            name,
            annotation,
            value,
            span,
        } => Expr::VarDecl {
            mutable,
            name,
            annotation,
            value: Box::new(replace_direct_child(*value, counter)),
            span,
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
            name,
            type_params,
            constraints,
            params,
            param_annotations,
            return_annotation,
            body: Box::new(replace_direct_child(*body, counter)),
            span,
        },
        Expr::Lambda {
            params,
            param_annotations,
            body,
            span,
        } => Expr::Lambda {
            params,
            param_annotations,
            body: Box::new(replace_direct_child(*body, counter)),
            span,
        },
        Expr::Assign { name, value, span } => Expr::Assign {
            name,
            value: Box::new(replace_direct_child(*value, counter)),
            span,
        },
        Expr::Unary { op, expr, span } => Expr::Unary {
            op,
            expr: Box::new(replace_direct_child(*expr, counter)),
            span,
        },
        Expr::Binary { lhs, op, rhs, span } => Expr::Binary {
            lhs: Box::new(replace_direct_child(*lhs, counter)),
            op,
            rhs: Box::new(replace_direct_child(*rhs, counter)),
            span,
        },
        Expr::Call {
            callee,
            arguments,
            span,
        } => Expr::Call {
            callee: Box::new(replace_direct_child(*callee, counter)),
            arguments: arguments
                .into_iter()
                .map(|argument| replace_direct_child(argument, counter))
                .collect(),
            span,
        },
        Expr::FieldAccess {
            target,
            field,
            span,
        } => Expr::FieldAccess {
            target: Box::new(replace_direct_child(*target, counter)),
            field,
            span,
        },
        Expr::Cleanup {
            body,
            cleanup,
            span,
        } => Expr::Cleanup {
            body: Box::new(replace_direct_child(*body, counter)),
            cleanup: Box::new(replace_direct_child(*cleanup, counter)),
            span,
        },
        Expr::RecordConstructor {
            name,
            arguments,
            span,
        } => Expr::RecordConstructor {
            name,
            arguments: arguments
                .into_iter()
                .map(|argument| replace_direct_child(argument, counter))
                .collect(),
            span,
        },
        Expr::ListLiteral { elements, span } => Expr::ListLiteral {
            elements: elements
                .into_iter()
                .map(|element| replace_direct_child(element, counter))
                .collect(),
            span,
        },
        Expr::MapLiteral { entries, span } => Expr::MapLiteral {
            entries: entries
                .into_iter()
                .map(|(key, value)| {
                    (
                        replace_direct_child(key, counter),
                        replace_direct_child(value, counter),
                    )
                })
                .collect(),
            span,
        },
        Expr::SetLiteral { elements, span } => Expr::SetLiteral {
            elements: elements
                .into_iter()
                .map(|element| replace_direct_child(element, counter))
                .collect(),
            span,
        },
        Expr::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => Expr::If {
            condition: Box::new(replace_direct_child(*condition, counter)),
            then_branch: Box::new(replace_direct_child(*then_branch, counter)),
            else_branch: else_branch.map(|branch| Box::new(replace_direct_child(*branch, counter))),
            span,
        },
        Expr::While {
            condition,
            body,
            span,
        } => Expr::While {
            condition: Box::new(replace_direct_child(*condition, counter)),
            body: Box::new(replace_direct_child(*body, counter)),
            span,
        },
        Expr::Foreach {
            binding,
            iterable,
            body,
            span,
        } => Expr::Foreach {
            binding,
            iterable: Box::new(replace_direct_child(*iterable, counter)),
            body: Box::new(replace_direct_child(*body, counter)),
            span,
        },
        Expr::Block { expressions, span } => Expr::Block {
            expressions: expressions
                .into_iter()
                .map(|expression| replace_direct_child(expression, counter))
                .collect(),
            span,
        },
        Expr::StringInterpolation { parts, span } => Expr::StringInterpolation {
            parts: parts
                .into_iter()
                .map(|part| match part {
                    StringPart::Literal(text) => StringPart::Literal(text),
                    StringPart::Interpolation(hole) => {
                        StringPart::Interpolation(Box::new(replace_direct_child(*hole, counter)))
                    }
                })
                .collect(),
            span,
        },
        other => other,
    }
}

/// Replace `expr` itself with a fresh placeholder name if it is a bare `_`,
/// otherwise leave it untouched. Unlike `replace_placeholders`, this does
/// not recurse: every non-leaf `expr` reaching here was already produced by
/// `rewrite` (directly, or via `finalize`), which means any placeholder
/// nested inside it was already found and converted into a `Lambda` while
/// that sub-expression was itself being rewritten. See the comment on
/// `replace_placeholders` for why a deeper walk here would be redundant.
fn replace_direct_child(expr: Expr, counter: &mut usize) -> Expr {
    match expr {
        Expr::Identifier { name, span } if name == "_" => {
            let name = format!("__placeholder{}", *counter);
            *counter += 1;
            Expr::Identifier { name, span }
        }
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::rewrite_expression;
    use klassic_span::{SourceFile, Span};
    use klassic_syntax::{Expr, IntLiteralKind, parse_source};

    #[test]
    fn thin_slice_rewrite_is_identity() {
        let _file = SourceFile::new("test.kl", "1");
        let expr = Expr::Int {
            value: 1,
            kind: IntLiteralKind::Int,
            span: Span::new(0, 1),
        };
        assert_eq!(rewrite_expression(expr.clone()), expr);
    }

    #[test]
    fn desugars_single_placeholder_into_lambda() {
        let parsed = parse_source(&SourceFile::new("test.kl", "_ + 1")).expect("should parse");
        let rewritten = rewrite_expression(parsed);
        match rewritten {
            Expr::Lambda { params, .. } => assert_eq!(params, vec!["__placeholder0"]),
            other => panic!("unexpected rewritten expression: {other:?}"),
        }
    }

    #[test]
    fn desugars_multiple_placeholders_left_to_right() {
        let parsed = parse_source(&SourceFile::new("test.kl", "_ + _")).expect("should parse");
        let rewritten = rewrite_expression(parsed);
        match rewritten {
            Expr::Lambda { params, body, .. } => {
                assert_eq!(params, vec!["__placeholder0", "__placeholder1"]);
                match *body {
                    Expr::Binary { .. } => {}
                    other => panic!("unexpected body: {other:?}"),
                }
            }
            other => panic!("unexpected rewritten expression: {other:?}"),
        }
    }
}
