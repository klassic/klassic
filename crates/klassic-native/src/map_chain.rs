//! Lower a map that has to grow while the program runs onto a chain.
//!
//! The builtin `Map` is a compile-time value on the x86-64 backend and a
//! chain of cells on the aarch64 one, which is why `Map#put` in a loop works
//! on one and not the other. This pass rewrites such a map onto a
//! self-referential enum and the helpers in [`HELPERS`], which every backend
//! compiles (see `tests/cross-exec/32-map-chain-helpers.kl`).
//!
//! It is deliberately narrow. A map is lowered only when it is *created* by
//! `Map#empty()`, and only when every use of it is one of the operations the
//! helpers provide. Anything else -- handing the map to a function, asking for
//! its keys, walking its entries -- leaves the whole program alone, so a
//! program that compiles today keeps compiling the way it did.

use std::collections::HashSet;

use klassic_span::Span;
use klassic_syntax::{Expr, MatchArm, StringPart};

/// The chain and its helpers, prepended to the prelude when a program needs
/// them. Written with `while` rather than recursion so every backend can
/// compile them: a recursive generic function cannot be compiled once, and
/// these are generic in the key and the value.
///
/// The names avoid a leading `__` because a constructor whose name starts
/// with one does not parse in a pattern.
pub const HELPERS: &str = r#"
enum KlassicMapChain<'k, 'v> {
  case KlassicMapNil
  case KlassicMapCons(key: 'k, value: 'v, rest: KlassicMapChain<'k, 'v>)
}

def klassicMapReverse(m: KlassicMapChain<'k, 'v>): KlassicMapChain<'k, 'v> = {
  mutable cur = m
  mutable acc = KlassicMapNil
  mutable go = true
  while (go) {
    cur match {
      case KlassicMapNil => { go = false; 0 }
      case KlassicMapCons(k, v, rest) => { acc = KlassicMapCons(k, v, acc); cur = rest; 0 }
    }
  }
  acc
}

def klassicMapPut(m: KlassicMapChain<'k, 'v>, key: 'k, value: 'v): KlassicMapChain<'k, 'v> = {
  mutable cur = m
  mutable acc = KlassicMapNil
  mutable replaced = false
  mutable go = true
  while (go) {
    cur match {
      case KlassicMapNil => { go = false; 0 }
      case KlassicMapCons(k, v, rest) => {
        acc = (if (k == key) KlassicMapCons(k, value, acc) else KlassicMapCons(k, v, acc))
        replaced = (if (k == key) true else replaced)
        cur = rest
        0
      }
    }
  }
  klassicMapReverse((if (replaced) acc else KlassicMapCons(key, value, acc)))
}

def klassicMapGetOrElse(m: KlassicMapChain<'k, 'v>, key: 'k, fallback: 'v): 'v = {
  mutable cur = m
  mutable found = fallback
  mutable go = true
  while (go) {
    cur match {
      case KlassicMapNil => { go = false; 0 }
      case KlassicMapCons(k, v, rest) => {
        found = (if (k == key) v else found)
        go = (if (k == key) false else true)
        cur = rest
        0
      }
    }
  }
  found
}

def klassicMapContainsKey(m: KlassicMapChain<'k, 'v>, key: 'k): Boolean = {
  mutable cur = m
  mutable yes = false
  mutable go = true
  while (go) {
    cur match {
      case KlassicMapNil => { go = false; 0 }
      case KlassicMapCons(k, v, rest) => {
        yes = (if (k == key) true else yes)
        go = (if (k == key) false else true)
        cur = rest
        0
      }
    }
  }
  yes
}

def klassicMapSize(m: KlassicMapChain<'k, 'v>): Int = {
  mutable cur = m
  mutable n = 0
  mutable go = true
  while (go) {
    cur match {
      case KlassicMapNil => { go = false; 0 }
      case KlassicMapCons(k, v, rest) => { n = n + 1; cur = rest; 0 }
    }
  }
  n
}

def klassicMapRender(m: KlassicMapChain<'k, 'v>): String = {
  mutable cur = m
  mutable out = ""
  mutable first = true
  mutable go = true
  while (go) {
    cur match {
      case KlassicMapNil => { go = false; 0 }
      case KlassicMapCons(k, v, rest) => {
        out = (if (first) (out + k + ": " + v) else (out + ", " + k + ": " + v))
        first = false
        cur = rest
        0
      }
    }
  }
  "%[" + out + "]"
}
"#;

/// Does this program create a map with `Map#empty()`? Asked of the user's own
/// text, before parsing, to decide whether [`HELPERS`] is worth prepending --
/// a program that never grows a map should not carry them.
pub fn program_creates_runtime_map(user_text: &str) -> bool {
    user_text.contains("Map#empty")
}

/// Rewrite the maps this pass can lower, or leave the program exactly as it
/// was. `prelude_len` is where the user's own text begins; only the user's
/// code is rewritten, because the stdlib's own map functions operate on the
/// builtin map and are compiled the way they always were.
pub fn lower_runtime_maps(expr: Expr, prelude_len: usize) -> Expr {
    let mut tracked = HashSet::new();
    collect_created_maps(&expr, prelude_len, &mut tracked);
    if tracked.is_empty() {
        return expr;
    }
    if !every_use_is_lowerable(&expr, prelude_len, &tracked) {
        return expr;
    }
    rewrite(expr, prelude_len, &tracked)
}

fn is_user(span: Span, prelude_len: usize) -> bool {
    span.start >= prelude_len
}

/// Names bound to `Map#empty()` in the user's own code.
fn collect_created_maps(expr: &Expr, prelude_len: usize, out: &mut HashSet<String>) {
    if let Expr::VarDecl {
        name, value, span, ..
    } = expr
        && is_user(*span, prelude_len)
        && is_map_empty_call(value)
    {
        out.insert(name.clone());
    }
    for child in children(expr) {
        collect_created_maps(child, prelude_len, out);
    }
}

fn is_map_empty_call(expr: &Expr) -> bool {
    matches!(expr, Expr::Call { callee, arguments, .. }
        if arguments.is_empty()
            && matches!(callee.as_ref(), Expr::Identifier { name, .. } if name == "Map#empty"))
}

/// Every mention of a tracked name has to be one of the shapes `rewrite`
/// knows. One that is not leaves the whole program alone rather than half
/// lowering it.
fn every_use_is_lowerable(expr: &Expr, prelude_len: usize, tracked: &HashSet<String>) -> bool {
    match expr {
        // A bare mention is fine only where `rewrite` handles it: as the map
        // of a supported call, as a `println` argument, or as the value of an
        // assignment to a tracked name. Those cases are checked by their
        // parents below, so a mention reaching here is not lowerable.
        Expr::Identifier { name, span } => !(is_user(*span, prelude_len) && tracked.contains(name)),
        Expr::VarDecl {
            name, value, span, ..
        } => {
            if is_user(*span, prelude_len) && tracked.contains(name) && is_map_empty_call(value) {
                return true;
            }
            every_use_is_lowerable(value, prelude_len, tracked)
        }
        Expr::Assign { name, value, span } => {
            if is_user(*span, prelude_len) && tracked.contains(name) {
                return lowerable_map_expr(value, prelude_len, tracked);
            }
            every_use_is_lowerable(value, prelude_len, tracked)
        }
        Expr::Call {
            callee, arguments, ..
        } => {
            if let Some(parts) = supported_call(callee, arguments, tracked, prelude_len) {
                return parts
                    .iter()
                    .all(|part| every_use_is_lowerable(part, prelude_len, tracked));
            }
            every_use_is_lowerable(callee, prelude_len, tracked)
                && arguments
                    .iter()
                    .all(|argument| every_use_is_lowerable(argument, prelude_len, tracked))
        }
        other => children(other)
            .into_iter()
            .all(|child| every_use_is_lowerable(child, prelude_len, tracked)),
    }
}

/// An expression that may stand for a lowered map: the empty map, or a `put`
/// on one.
fn lowerable_map_expr(expr: &Expr, prelude_len: usize, tracked: &HashSet<String>) -> bool {
    if is_map_empty_call(expr) {
        return true;
    }
    let Expr::Call {
        callee, arguments, ..
    } = expr
    else {
        return false;
    };
    let Expr::Identifier { name, .. } = callee.as_ref() else {
        return false;
    };
    if name != "Map#put" || arguments.len() != 3 {
        return false;
    }
    matches!(&arguments[0], Expr::Identifier { name, .. } if tracked.contains(name))
        && every_use_is_lowerable(&arguments[1], prelude_len, tracked)
        && every_use_is_lowerable(&arguments[2], prelude_len, tracked)
}

/// The arguments that still need checking when a call is one this pass
/// rewrites -- the map itself is accounted for by the rewrite.
fn supported_call<'a>(
    callee: &'a Expr,
    arguments: &'a [Expr],
    tracked: &HashSet<String>,
    prelude_len: usize,
) -> Option<Vec<&'a Expr>> {
    let names_tracked_map = |expr: &Expr| {
        matches!(expr, Expr::Identifier { name, span }
            if tracked.contains(name) && is_user(*span, prelude_len))
    };
    match callee {
        Expr::Identifier { name, .. } => {
            let rest: Vec<&Expr> = arguments.iter().skip(1).collect();
            match (name.as_str(), arguments.len()) {
                ("Map#put", 3) | ("Map#getOrElse", 3) | ("getOrElse", 3)
                    if names_tracked_map(&arguments[0]) =>
                {
                    Some(rest)
                }
                ("Map#containsKey", 2) | ("containsKey", 2) if names_tracked_map(&arguments[0]) => {
                    Some(rest)
                }
                ("Map#size", 1) | ("size", 1) | ("Map#isEmpty", 1)
                    if names_tracked_map(&arguments[0]) =>
                {
                    Some(Vec::new())
                }
                ("println", 1) | ("printlnError", 1) if names_tracked_map(&arguments[0]) => {
                    Some(Vec::new())
                }
                _ => None,
            }
        }
        // `m.getOrElse(k, d)`, `m.containsKey(k)`, `m.sizeOf()`: the receiver
        // is the map and the method is one of the helpers.
        Expr::FieldAccess { target, field, .. } if names_tracked_map(target) => {
            match (field.as_str(), arguments.len()) {
                ("getOrElse", 2) | ("containsKey", 1) | ("sizeOf", 0) | ("isEmptyMap", 0) => {
                    Some(arguments.iter().collect())
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn call(name: &str, arguments: Vec<Expr>, span: Span) -> Expr {
    Expr::Call {
        callee: Box::new(Expr::Identifier {
            name: name.to_string(),
            span,
        }),
        arguments,
        span,
    }
}

fn rewrite(expr: Expr, prelude_len: usize, tracked: &HashSet<String>) -> Expr {
    let names_tracked_map = |expr: &Expr| {
        matches!(expr, Expr::Identifier { name, span }
            if tracked.contains(name) && is_user(*span, prelude_len))
    };
    match expr {
        Expr::Call {
            callee,
            arguments,
            span,
        } => {
            if is_map_empty_call(&Expr::Call {
                callee: callee.clone(),
                arguments: arguments.clone(),
                span,
            }) && is_user(span, prelude_len)
            {
                return Expr::Identifier {
                    name: "KlassicMapNil".to_string(),
                    span,
                };
            }
            let rewritten: Vec<Expr> = arguments
                .iter()
                .cloned()
                .map(|argument| rewrite(argument, prelude_len, tracked))
                .collect();
            if let Expr::Identifier { name, .. } = callee.as_ref()
                && !arguments.is_empty()
                && names_tracked_map(&arguments[0])
            {
                let helper = match (name.as_str(), arguments.len()) {
                    ("Map#put", 3) => Some("klassicMapPut"),
                    ("Map#getOrElse", 3) | ("getOrElse", 3) => Some("klassicMapGetOrElse"),
                    ("Map#containsKey", 2) | ("containsKey", 2) => Some("klassicMapContainsKey"),
                    ("Map#size", 1) | ("size", 1) => Some("klassicMapSize"),
                    _ => None,
                };
                if let Some(helper) = helper {
                    return call(helper, rewritten, span);
                }
                if name == "Map#isEmpty" && arguments.len() == 1 {
                    return Expr::Binary {
                        lhs: Box::new(call("klassicMapSize", rewritten, span)),
                        op: klassic_syntax::BinaryOp::Equal,
                        rhs: Box::new(Expr::Int {
                            value: 0,
                            kind: klassic_syntax::IntLiteralKind::Int,
                            span,
                        }),
                        span,
                    };
                }
                if matches!(name.as_str(), "println" | "printlnError") && arguments.len() == 1 {
                    return call(name, vec![call("klassicMapRender", rewritten, span)], span);
                }
            }
            if let Expr::FieldAccess { target, field, .. } = callee.as_ref()
                && names_tracked_map(target)
            {
                let mut all = vec![rewrite((**target).clone(), prelude_len, tracked)];
                all.extend(rewritten.iter().cloned());
                match (field.as_str(), arguments.len()) {
                    ("getOrElse", 2) => return call("klassicMapGetOrElse", all, span),
                    ("containsKey", 1) => return call("klassicMapContainsKey", all, span),
                    ("sizeOf", 0) => return call("klassicMapSize", all, span),
                    ("isEmptyMap", 0) => {
                        return Expr::Binary {
                            lhs: Box::new(call("klassicMapSize", all, span)),
                            op: klassic_syntax::BinaryOp::Equal,
                            rhs: Box::new(Expr::Int {
                                value: 0,
                                kind: klassic_syntax::IntLiteralKind::Int,
                                span,
                            }),
                            span,
                        };
                    }
                    _ => {}
                }
            }
            Expr::Call {
                callee: Box::new(rewrite(*callee, prelude_len, tracked)),
                arguments: rewritten,
                span,
            }
        }
        other => map_children(other, &mut |child| rewrite(child, prelude_len, tracked)),
    }
}

/// The children of an expression, for the walks above.
fn children(expr: &Expr) -> Vec<&Expr> {
    match expr {
        Expr::Block { expressions, .. } => expressions.iter().collect(),
        Expr::Call {
            callee, arguments, ..
        } => std::iter::once(callee.as_ref())
            .chain(arguments.iter())
            .collect(),
        Expr::Binary { lhs, rhs, .. } => vec![lhs.as_ref(), rhs.as_ref()],
        Expr::Unary { expr, .. } => vec![expr.as_ref()],
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let mut out = vec![condition.as_ref(), then_branch.as_ref()];
            if let Some(branch) = else_branch {
                out.push(branch.as_ref());
            }
            out
        }
        Expr::While {
            condition, body, ..
        } => vec![condition.as_ref(), body.as_ref()],
        Expr::Foreach { iterable, body, .. } => vec![iterable.as_ref(), body.as_ref()],
        Expr::VarDecl { value, .. }
        | Expr::Assign { value, .. }
        | Expr::Lambda { body: value, .. }
        | Expr::DefDecl { body: value, .. } => vec![value.as_ref()],
        Expr::Cleanup { body, cleanup, .. } => vec![body.as_ref(), cleanup.as_ref()],
        Expr::Match {
            scrutinee, arms, ..
        } => {
            let mut out = vec![scrutinee.as_ref()];
            out.extend(arms.iter().map(|arm| &arm.body));
            out
        }
        Expr::ListLiteral { elements, .. } | Expr::SetLiteral { elements, .. } => {
            elements.iter().collect()
        }
        Expr::RecordConstructor { arguments, .. } => arguments.iter().collect(),
        Expr::FieldAccess { target, .. } => vec![target.as_ref()],
        Expr::RecordLiteral { fields, .. } => fields.iter().map(|(_, value)| value).collect(),
        Expr::MapLiteral { entries, .. } => entries
            .iter()
            .flat_map(|(key, value)| [key, value])
            .collect(),
        Expr::InstanceDeclaration { methods, .. } => methods.iter().collect(),
        Expr::ExtensionDeclaration { methods, .. } => methods.iter().collect(),
        // Declarations and literals with nothing to walk. Listed rather than
        // caught by a wildcard, so a new kind of expression is a compile error
        // here instead of a silently unchecked one -- this walk deciding "no
        // children" for something that has them is how a use slips past the
        // check that keeps the lowering all-or-nothing.
        Expr::Int { .. }
        | Expr::Double { .. }
        | Expr::Bool { .. }
        | Expr::String { .. }
        | Expr::Null { .. }
        | Expr::Unit { .. }
        | Expr::Identifier { .. }
        | Expr::ModuleHeader { .. }
        | Expr::Import { .. }
        | Expr::RecordDeclaration { .. }
        | Expr::TypeClassDeclaration { .. }
        | Expr::TheoremDeclaration { .. }
        | Expr::AxiomDeclaration { .. }
        | Expr::EnumDeclaration { .. }
        | Expr::PegRuleBlock { .. } => Vec::new(),
        Expr::StringInterpolation { parts, .. } => parts
            .iter()
            .filter_map(|part| match part {
                StringPart::Interpolation(hole) => Some(hole.as_ref()),
                StringPart::Literal(_) => None,
            })
            .collect(),
    }
}

/// Rebuild an expression with each child mapped, for the shapes `rewrite`
/// does not handle itself.
fn map_children(expr: Expr, f: &mut impl FnMut(Expr) -> Expr) -> Expr {
    match expr {
        Expr::Block { expressions, span } => Expr::Block {
            expressions: expressions.into_iter().map(&mut *f).collect(),
            span,
        },
        Expr::Binary { lhs, op, rhs, span } => Expr::Binary {
            lhs: Box::new(f(*lhs)),
            op,
            rhs: Box::new(f(*rhs)),
            span,
        },
        Expr::Unary { op, expr, span } => Expr::Unary {
            op,
            expr: Box::new(f(*expr)),
            span,
        },
        Expr::If {
            condition,
            then_branch,
            else_branch,
            span,
        } => Expr::If {
            condition: Box::new(f(*condition)),
            then_branch: Box::new(f(*then_branch)),
            else_branch: else_branch.map(|branch| Box::new(f(*branch))),
            span,
        },
        Expr::While {
            condition,
            body,
            span,
        } => Expr::While {
            condition: Box::new(f(*condition)),
            body: Box::new(f(*body)),
            span,
        },
        Expr::Foreach {
            binding,
            iterable,
            body,
            span,
        } => Expr::Foreach {
            binding,
            iterable: Box::new(f(*iterable)),
            body: Box::new(f(*body)),
            span,
        },
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
            value: Box::new(f(*value)),
            span,
        },
        Expr::Assign { name, value, span } => Expr::Assign {
            name,
            value: Box::new(f(*value)),
            span,
        },
        Expr::Cleanup {
            body,
            cleanup,
            span,
        } => Expr::Cleanup {
            body: Box::new(f(*body)),
            cleanup: Box::new(f(*cleanup)),
            span,
        },
        Expr::Match {
            scrutinee,
            arms,
            span,
        } => Expr::Match {
            scrutinee: Box::new(f(*scrutinee)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    guard: arm.guard,
                    body: f(arm.body),
                    span: arm.span,
                })
                .collect(),
            span,
        },
        Expr::ListLiteral { elements, span } => Expr::ListLiteral {
            elements: elements.into_iter().map(&mut *f).collect(),
            span,
        },
        Expr::SetLiteral { elements, span } => Expr::SetLiteral {
            elements: elements.into_iter().map(&mut *f).collect(),
            span,
        },
        Expr::StringInterpolation { parts, span } => Expr::StringInterpolation {
            parts: parts
                .into_iter()
                .map(|part| match part {
                    StringPart::Interpolation(hole) => {
                        StringPart::Interpolation(Box::new(f(*hole)))
                    }
                    literal => literal,
                })
                .collect(),
            span,
        },
        other => other,
    }
}
