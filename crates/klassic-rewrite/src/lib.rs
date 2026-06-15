use klassic_syntax::{Expr, StringPart};

pub fn rewrite_expression(expr: Expr) -> Expr {
    finalize(expr)
}

fn finalize(expr: Expr) -> Expr {
    let span = expr.span();
    wrap_placeholders(rewrite(expr), span)
}

fn rewrite(expr: Expr) -> Expr {
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
                .map(|method| replace_placeholders(method, counter))
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
            value: Box::new(replace_placeholders(*value, counter)),
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
            body: Box::new(replace_placeholders(*body, counter)),
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
            body: Box::new(replace_placeholders(*body, counter)),
            span,
        },
        Expr::Assign { name, value, span } => Expr::Assign {
            name,
            value: Box::new(replace_placeholders(*value, counter)),
            span,
        },
        Expr::Unary { op, expr, span } => Expr::Unary {
            op,
            expr: Box::new(replace_placeholders(*expr, counter)),
            span,
        },
        Expr::Binary { lhs, op, rhs, span } => Expr::Binary {
            lhs: Box::new(replace_placeholders(*lhs, counter)),
            op,
            rhs: Box::new(replace_placeholders(*rhs, counter)),
            span,
        },
        Expr::Call {
            callee,
            arguments,
            span,
        } => Expr::Call {
            callee: Box::new(replace_placeholders(*callee, counter)),
            arguments: arguments
                .into_iter()
                .map(|argument| replace_placeholders(argument, counter))
                .collect(),
            span,
        },
        Expr::FieldAccess {
            target,
            field,
            span,
        } => Expr::FieldAccess {
            target: Box::new(replace_placeholders(*target, counter)),
            field,
            span,
        },
        Expr::Cleanup {
            body,
            cleanup,
            span,
        } => Expr::Cleanup {
            body: Box::new(replace_placeholders(*body, counter)),
            cleanup: Box::new(replace_placeholders(*cleanup, counter)),
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
                .map(|argument| replace_placeholders(argument, counter))
                .collect(),
            span,
        },
        Expr::ListLiteral { elements, span } => Expr::ListLiteral {
            elements: elements
                .into_iter()
                .map(|element| replace_placeholders(element, counter))
                .collect(),
            span,
        },
        Expr::MapLiteral { entries, span } => Expr::MapLiteral {
            entries: entries
                .into_iter()
                .map(|(key, value)| {
                    (
                        replace_placeholders(key, counter),
                        replace_placeholders(value, counter),
                    )
                })
                .collect(),
            span,
        },
        Expr::SetLiteral { elements, span } => Expr::SetLiteral {
            elements: elements
                .into_iter()
                .map(|element| replace_placeholders(element, counter))
                .collect(),
            span,
        },
        Expr::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => Expr::If {
            condition: Box::new(replace_placeholders(*condition, counter)),
            then_branch: Box::new(replace_placeholders(*then_branch, counter)),
            else_branch: else_branch.map(|branch| Box::new(replace_placeholders(*branch, counter))),
            span,
        },
        Expr::While {
            condition,
            body,
            span,
        } => Expr::While {
            condition: Box::new(replace_placeholders(*condition, counter)),
            body: Box::new(replace_placeholders(*body, counter)),
            span,
        },
        Expr::Foreach {
            binding,
            iterable,
            body,
            span,
        } => Expr::Foreach {
            binding,
            iterable: Box::new(replace_placeholders(*iterable, counter)),
            body: Box::new(replace_placeholders(*body, counter)),
            span,
        },
        Expr::Block { expressions, span } => Expr::Block {
            expressions: expressions
                .into_iter()
                .map(|expression| replace_placeholders(expression, counter))
                .collect(),
            span,
        },
        Expr::StringInterpolation { parts, span } => Expr::StringInterpolation {
            parts: parts
                .into_iter()
                .map(|part| match part {
                    StringPart::Literal(text) => StringPart::Literal(text),
                    StringPart::Interpolation(hole) => {
                        StringPart::Interpolation(Box::new(replace_placeholders(*hole, counter)))
                    }
                })
                .collect(),
            span,
        },
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
