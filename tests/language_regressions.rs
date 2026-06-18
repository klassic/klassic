use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use klassic_eval::{Evaluator, EvaluatorConfig, Value, evaluate_text, evaluate_text_with_config};

fn unique_temp_path(name: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be after epoch")
        .as_nanos();
    std::env::temp_dir().join(format!(
        "klassic-rust-{name}-{}-{nanos}",
        std::process::id()
    ))
}

fn path_string(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

#[test]
fn literal_specs_are_covered() {
    assert_eq!(evaluate_text("<expr>", "2").unwrap(), Value::Int(2));
    assert!(matches!(
        evaluate_text("<expr>", "2L").unwrap(),
        Value::Long(2)
    ));
    assert!(matches!(
        evaluate_text("<expr>", "2.5F").unwrap(),
        Value::Float(value) if (value - 2.5).abs() < f32::EPSILON
    ));
    assert_eq!(evaluate_text("<expr>", "2.5").unwrap(), Value::Double(2.5));
    assert_eq!(
        evaluate_text("<expr>", r#""\r\n""#).unwrap(),
        Value::String("\r\n".to_string())
    );
    assert_eq!(evaluate_text("<expr>", "true").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_text("<expr>", "()").unwrap(), Value::Unit);
    assert_eq!(
        evaluate_text("<expr>", "[1 2\n 3 4]").unwrap(),
        Value::List(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4)
        ])
    );
    assert_eq!(
        evaluate_text("<expr>", r#"%["a":"b" "c":"d"]"#).unwrap(),
        Value::Map(vec![
            (
                Value::String("a".to_string()),
                Value::String("b".to_string())
            ),
            (
                Value::String("c".to_string()),
                Value::String("d".to_string())
            ),
        ])
    );
    assert_eq!(
        evaluate_text("<expr>", "%(1 2\n 3)").unwrap(),
        Value::Set(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
    );
    assert_eq!(
        evaluate_text("<expr>", "val x = 2\n\"value=#{x}\"").unwrap(),
        Value::String("value=2".to_string())
    );
}

#[test]
fn function_specs_are_covered() {
    assert_eq!(evaluate_text("<expr>", "ceil(2.5)").unwrap(), Value::Int(3));
    assert_eq!(
        evaluate_text("<expr>", "int(-1.5)").unwrap(),
        Value::Int(-1)
    );
    assert_eq!(
        evaluate_text("<expr>", "double(3)").unwrap(),
        Value::Double(3.0)
    );
    assert_eq!(
        evaluate_text("<expr>", "floor(2.5)").unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text("<expr>", "abs(-1.5)").unwrap(),
        Value::Double(1.5)
    );
    assert_eq!(
        evaluate_text("<expr>", r#"substring("FOO", 1, 3)"#).unwrap(),
        Value::String("OO".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", r#"at("FOO", 1)"#).unwrap(),
        Value::String("O".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", r#"matches("FOO", "FO")"#).unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        evaluate_text("<expr>", "sqrt(4.0)").unwrap(),
        Value::Double(2.0)
    );
}

#[test]
fn expression_specs_are_covered() {
    assert_eq!(
        evaluate_text("<expr>", "mutable a=1\na += 1\na").unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "mutable i = 10\nwhile(i >= 0) {\n  i = i - 1\n}\ni",
        )
        .unwrap(),
        Value::Int(-1)
    );
    assert_eq!(
        evaluate_text("<expr>", "val add = (x, y) => x + y\nadd(3, 3)").unwrap(),
        Value::Int(6)
    );
    assert_eq!(
        evaluate_text("<expr>", "val i = -1\ni < 0 || i > 10").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "mutable sum = 0\nforeach(a in [1, 2, 3, 4, 5]) {\n  sum += a\n}\nsum",
        )
        .unwrap(),
        Value::Int(15)
    );
    assert_eq!(
        evaluate_text("<expr>", "if(false) 1.0 else 2.0").unwrap(),
        Value::Double(2.0)
    );
    assert_eq!(
        evaluate_text("<expr>", "val x = 2\nx < 2 then \"A\" else \"B\"").unwrap(),
        Value::String("B".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", "def add(x, y) = x + y\nadd(2, 3)").unwrap(),
        Value::Int(5)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def fact(n) = if(n < 2) 1 else (n * fact(n - 1))\nfact(4)"
        )
        .unwrap(),
        Value::Int(24)
    );
    assert_eq!(
        evaluate_text("<expr>", "def none() = 24 cleanup \"none\"\nnone()").unwrap(),
        Value::Int(24)
    );
}

#[test]
fn typechecker_specs_are_covered_more_directly() {
    assert_eq!(
        evaluate_text("<expr>", "val a=1\na").unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        evaluate_text("<expr>", "mutable a=1\na = a + 1\na").unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text("<expr>", "mutable s=\"FOO\"\ns=s+s\ns").unwrap(),
        Value::String("FOOFOO".to_string())
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def add(x: Int, y: Int): Int = x + y\nval f: (Int, Int) => Int = add\nf(2, 3)",
        )
        .unwrap(),
        Value::Int(5)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "mutable a = 1\na = 2\nforeach(b in [1, 2, 3]) {\n  (b :> Int) + 3\n}",
        )
        .unwrap(),
        Value::Unit
    );

    let mixed_integral = evaluate_text("<expr>", "val a = 1\nval b = 2L\n1 + 2L")
        .expect_err("mixed integral arithmetic should fail");
    assert!(mixed_integral.to_string().contains("type mismatch"));

    let incompatible_annotation = evaluate_text("<expr>", "val a = 1\nval b: Long = a\nb")
        .expect_err("incompatible annotation should fail");
    assert!(
        incompatible_annotation
            .to_string()
            .contains("type mismatch")
    );

    let invalid_foreach =
        evaluate_text("<expr>", "val a = 1\nforeach(a in [1, 2, 3]) {\n  b + 3\n}")
            .expect_err("undefined foreach body variable should fail");
    assert!(
        invalid_foreach
            .to_string()
            .contains("undefined variable `b`")
    );

    let immutable_assignment =
        evaluate_text("<expr>", "val a = 1\na = 2\na").expect_err("assignment to val should fail");
    assert!(immutable_assignment.to_string().contains("immutable"));

    let partial_call = evaluate_text("<expr>", "def f(x, y) = x + y\nf(10)")
        .expect_err("non-curried partial function call should fail");
    assert!(
        partial_call
            .to_string()
            .contains("function expects 2 arguments but got 1")
    );

    // A bare lowercase `def` type parameter (`def id<a>(x: a): a`) is a type
    // variable, consistent with enums/records/extensions. It used to be
    // treated as a concrete type, so even `id(5)` failed to unify; only the
    // `'a` form worked.
    assert_eq!(
        evaluate_text("<expr>", "def id<a>(x: a): a = x\nid(5)").unwrap(),
        Value::Int(5)
    );
    assert_eq!(
        evaluate_text("<expr>", "def id<a>(x: a): a = x\nid(id(5))").unwrap(),
        Value::Int(5)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def const<a, b>(x: a, y: b): a = x\nconst(1, \"ignored\")",
        )
        .unwrap(),
        Value::Int(1)
    );

    // Over-application: a curried function applied to more arguments than its
    // first arity feeds the rest to the returned function, like the runtime.
    // `map(xs, f)` used to be rejected ("expects 1 argument but got 2") even
    // though it ran; it now type-checks like `map(xs)(f)`.
    assert_eq!(
        evaluate_text("<expr>", "map([1 2 3], (x) => x + 1)").unwrap(),
        Value::List(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
    );
    // A genuine too-many-arguments error (a non-function result) is still
    // reported as an arity error, not silently over-applied.
    let too_many = evaluate_text("<expr>", "def f(x: Int): Int = x\nf(1, 2)")
        .expect_err("calling a scalar-returning function with extra args should fail");
    assert!(
        too_many
            .to_string()
            .contains("expects 1 argument but got 2")
    );
    // Partial application (too few arguments) is still rejected.
    let too_few = evaluate_text("<expr>", "def g(x, y) = x + y\ng(1)")
        .expect_err("partial application should fail");
    assert!(
        too_few
            .to_string()
            .contains("expects 2 arguments but got 1")
    );

    // Non-lambda arguments are checked before lambda arguments, so a lambda
    // whose body dispatches a method on its parameter (`(lst) => lst.size()`)
    // sees the parameter's type pinned by the sibling argument. This used to
    // commit the parameter to a structural-record shape and reject the call.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def hof(f: ('a) => 'b, x: 'a): 'b = f(x)\nhof((lst) => lst.size(), [1, 2, 3])",
        )
        .unwrap(),
        Value::Int(3)
    );
}

#[test]
fn placeholder_specs_are_covered_more_directly() {
    assert_eq!(
        evaluate_text("<expr>", "val xs = [1 2 3]\nmap(xs)(_ + 1)").unwrap(),
        Value::List(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
    );
    assert_eq!(
        evaluate_text("<expr>", "val add = _ + _\nfoldLeft([1 2 3])(0)(add)").unwrap(),
        Value::Int(6)
    );
    assert_eq!(
        evaluate_text("<expr>", "foldLeft([1 2 3])(0)(_ + _)").unwrap(),
        Value::Int(6)
    );
    assert_eq!(
        evaluate_text("<expr>", "map([1 2 3])(- _)").unwrap(),
        Value::List(vec![Value::Int(-1), Value::Int(-2), Value::Int(-3)])
    );
    assert_eq!(
        evaluate_text("<expr>", "map([1 2 3])(+ _)").unwrap(),
        Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
    );
    assert_eq!(
        evaluate_text("<expr>", "val id = _\nmap([1])(id)").unwrap(),
        Value::List(vec![Value::Int(1)])
    );
    assert_eq!(
        evaluate_text("<expr>", "def f(x) = _\nmap([1])(f(1))").unwrap(),
        Value::List(vec![Value::Int(1)])
    );
}

#[test]
fn binary_expression_specs_are_covered() {
    assert_eq!(evaluate_text("<expr>", "1 & 0").unwrap(), Value::Int(0));
    assert_eq!(evaluate_text("<expr>", "1 & 1").unwrap(), Value::Int(1));
    assert_eq!(evaluate_text("<expr>", "1 | 0").unwrap(), Value::Int(1));
    assert_eq!(evaluate_text("<expr>", "0 | 0").unwrap(), Value::Int(0));
    assert_eq!(evaluate_text("<expr>", "1 ^ 0").unwrap(), Value::Int(1));
    assert_eq!(evaluate_text("<expr>", "1 ^ 1").unwrap(), Value::Int(0));
    assert_eq!(evaluate_text("<expr>", "0 ^ 0").unwrap(), Value::Int(0));
}

#[test]
fn comment_specs_are_covered() {
    assert_eq!(
        evaluate_text("<expr>", "//line comment\n1").unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        evaluate_text("<expr>", "1 +\n//line comment\n2").unwrap(),
        Value::Int(3)
    );
    assert_eq!(
        evaluate_text("<expr>", "1\n//line comment\n+ 2").unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text("<expr>", "1 + /* block comment */\n1").unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text("<expr>", "/*/**/*/\n1").unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        evaluate_text("<expr>", "1 /* nested\n     /* comment */\n   here */ + 2",).unwrap(),
        Value::Int(3)
    );
}

#[test]
fn list_function_specs_are_covered_more_fully() {
    assert_eq!(
        evaluate_text("<expr>", "head([3 2 1])").unwrap(),
        Value::Int(3)
    );
    assert_eq!(
        evaluate_text("<expr>", "tail([3 2 1])").unwrap(),
        Value::List(vec![Value::Int(2), Value::Int(1)])
    );
    assert_eq!(
        evaluate_text("<expr>", "cons(3)([2, 1])").unwrap(),
        Value::List(vec![Value::Int(3), Value::Int(2), Value::Int(1)])
    );
    assert_eq!(
        evaluate_text("<expr>", "size([3 2 1])").unwrap(),
        Value::Int(3)
    );
    assert_eq!(
        evaluate_text("<expr>", "isEmpty([3 2 1])").unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        evaluate_text("<expr>", "map([1 2 3])((x) => x + 1)").unwrap(),
        Value::List(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
    );
    assert_eq!(
        evaluate_text("<expr>", "contains([1 2 3])(2)").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_text("<expr>", "val xs = [\"a\", \"b\"]\nxs.contains(\"c\")").unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        evaluate_text("<expr>", "[] map x => x + 1").unwrap(),
        Value::List(vec![])
    );
    assert_eq!(
        evaluate_text("<expr>", "[1 2 3 4] reduce 0 => r + e").unwrap(),
        Value::Int(10)
    );
}

#[test]
fn map_specs_are_covered() {
    assert_eq!(
        evaluate_text(
            "<expr>",
            "%[\"name\": \"Kota Mizushima\" \"age\": \"33\"] Map#containsKey \"name\"",
        )
        .unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "%[\"name\": \"Kota Mizushima\" \"age\": \"33\"] Map#containsValue \"33\"",
        )
        .unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "%[\"name\": \"Kota Mizushima\" \"age\": \"33\"] Map#get \"age\"",
        )
        .unwrap(),
        Value::String("33".to_string())
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "%[\"name\": \"Kota Mizushima\" \"age\": \"33\"] Map#get \"hoge\"",
        )
        .unwrap(),
        Value::Null
    );
    assert_eq!(
        evaluate_text("<expr>", "Map#isEmpty(%[])").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_text("<expr>", "Map#isEmpty(%[\"x\": 1 \"y\": 2])").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn exhaustiveness_checks_refutable_payload_subpatterns() {
    // A constructor arm with a refutable payload does not cover its whole
    // variant, so the match is non-exhaustive and rejected (it used to be
    // accepted and crash at runtime with `no match arm matched value`).
    let literal_payload = evaluate_text(
        "<expr>",
        "enum W { case Wrap(n: Int); case End }\n\
         def f(w: W): Int = w match { case Wrap(1) => 11; case End => 99 }\n\
         f(Wrap(42))",
    )
    .expect_err("a refutable literal payload should not cover the variant");
    assert!(literal_payload.to_string().contains("not exhaustive"));

    let nested_payload = evaluate_text(
        "<expr>",
        "enum Box { case Full(c: Box); case Empty }\n\
         def f(b: Box): Int = b match { case Full(Full(x)) => 1; case Empty => 2 }\n\
         f(Full(Empty))",
    )
    .expect_err("a nested constructor payload should not cover the variant");
    assert!(nested_payload.to_string().contains("not exhaustive"));

    // Valid coverage still type-checks: multiple arms jointly covering a
    // variant's payload, a sole-variant nested payload, a refutable arm
    // followed by a catch-all, and a Boolean payload matched on both values.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "enum O<a> { case Some(v: a); case None }\n\
             def f(o: O<O<Int>>): Int = o match { case Some(Some(v)) => v; case Some(None) => 0; case None => -1 }\n\
             f(Some(Some(7)))",
        )
        .unwrap(),
        Value::Int(7)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "enum Step { case St(a: Int) }\n\
             enum R<e> { case Good(s: Step); case Bad(m: e) }\n\
             def f(r: R<String>): Int = r match { case Good(St(a)) => a; case Bad(m) => 0 }\n\
             f(Good(St(9)))",
        )
        .unwrap(),
        Value::Int(9)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "enum W { case Wrap(n: Int); case End }\n\
             def f(w: W): Int = w match { case Wrap(1) => 11; case Wrap(x) => x; case End => 99 }\n\
             f(Wrap(42))",
        )
        .unwrap(),
        Value::Int(42)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "enum B { case Wrap(b: Boolean) }\n\
             def f(x: B): Int = x match { case Wrap(true) => 1; case Wrap(false) => 0 }\n\
             f(Wrap(true))",
        )
        .unwrap(),
        Value::Int(1)
    );
}

#[test]
fn bool_scrutinee_match_must_be_exhaustive() {
    // A `Boolean` has a finite domain, so a top-level match missing `true`
    // or `false` (and without a wildcard) is non-exhaustive and rejected at
    // type-check time — it used to be accepted and crash at runtime with
    // `no match arm matched value`, unlike the enum check.
    let missing_false = evaluate_text(
        "<expr>",
        "def f(b: Boolean): Int = b match { case true => 1 }\nf(false)",
    )
    .expect_err("a Boolean match missing `false` should be rejected");
    assert!(
        missing_false
            .to_string()
            .contains("not exhaustive: missing false"),
        "got: {missing_false}"
    );

    let missing_true = evaluate_text(
        "<expr>",
        "def f(b: Boolean): Int = b match { case false => 0 }\nf(true)",
    )
    .expect_err("a Boolean match missing `true` should be rejected");
    assert!(
        missing_true
            .to_string()
            .contains("not exhaustive: missing true"),
        "got: {missing_true}"
    );

    // A guard does not unconditionally cover its value, so a guarded `true`
    // arm leaves `true` uncovered.
    let guarded = evaluate_text(
        "<expr>",
        "def f(b: Boolean): Int = b match { case true if b => 1; case false => 0 }\nf(true)",
    )
    .expect_err("a guarded `true` arm does not cover `true`");
    assert!(
        guarded.to_string().contains("not exhaustive"),
        "got: {guarded}"
    );

    // Exhaustive forms still type-check (no false rejection): both literals,
    // either order, a wildcard, or a bare variable.
    for (program, expected) in [
        (
            "def f(b: Boolean): Int = b match { case true => 1; case false => 0 }\nf(true)",
            1,
        ),
        (
            "def f(b: Boolean): Int = b match { case false => 0; case true => 1 }\nf(true)",
            1,
        ),
        (
            "def f(b: Boolean): Int = b match { case true => 1; case _ => 0 }\nf(false)",
            0,
        ),
        (
            "def f(b: Boolean): Int = b match { case x => 99 }\nf(true)",
            99,
        ),
    ] {
        assert_eq!(
            evaluate_text("<expr>", program).unwrap(),
            Value::Int(expected),
            "exhaustive Boolean match should type-check: {program}"
        );
    }
}

#[test]
fn tostring_method_works_on_every_numeric_type() {
    // The `.toString()` value method was accepted on `Int` and `Double` but
    // not on `Long`, `Short`, `Byte`, or `Float`, even though the free
    // `toString(...)` function renders all of them — the method form failed
    // to type-check. They now match `Int`/`Double`.
    assert_eq!(
        evaluate_text("<expr>", "5L.toString()").unwrap(),
        Value::String("5".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", "2.5F.toString()").unwrap(),
        Value::String("2.5".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", "42.toString()").unwrap(),
        Value::String("42".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", "3.5.toString()").unwrap(),
        Value::String("3.5".to_string())
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def render(n: Long): String = n.toString()\nrender(42L)",
        )
        .unwrap(),
        Value::String("42".to_string())
    );
}

#[test]
fn repeated_literal_match_arm_is_unreachable() {
    // A repeated unguarded literal arm can never run — the constructor check
    // already rejected a duplicate `case A`, but a duplicate `case true`,
    // `case 1`, or `case "a"` used to be silently accepted as dead code.
    for (program, needle) in [
        (
            "def f(b: Boolean): Int = b match { case true => 1; case true => 2; case false => 0 }\nf(true)",
            "`true` is already matched",
        ),
        (
            "def f(n: Int): Int = n match { case 1 => 10; case 1 => 20; case _ => 0 }\nf(1)",
            "`1` is already matched",
        ),
        (
            "def f(s: String): Int = s match { case \"a\" => 1; case \"a\" => 2; case _ => 0 }\nf(\"a\")",
            "`\"a\"` is already matched",
        ),
    ] {
        let err = evaluate_text("<expr>", program)
            .expect_err("a repeated literal arm should be rejected as unreachable");
        assert!(
            err.to_string().contains("unreachable match arm") && err.to_string().contains(needle),
            "got: {err}"
        );
    }

    // Distinct literals, a guarded duplicate (the guard keeps the later arm
    // live), and the same literal across different scrutinee types all stay
    // accepted.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def f(n: Int): Int = n match { case 1 => 10; case 2 => 20; case _ => 0 }\nf(2)",
        )
        .unwrap(),
        Value::Int(20)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def f(n: Int): Int = n match { case 1 if false => 10; case 1 => 20; case _ => 0 }\nf(1)",
        )
        .unwrap(),
        Value::Int(20)
    );
}

#[test]
fn free_map_and_set_builtins_are_element_typed() {
    // `Map#keys` / `Map#values` / `Set#union` carry the collection's element
    // type instead of `Dynamic`, so a wrong-typed use is a type error rather
    // than a silent Dynamic escape.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "val vs: List<Int> = Map#values(%[\"a\": 1 \"b\": 2])\nfoldLeft(vs)(0)((acc, v) => acc + v)",
        )
        .unwrap(),
        Value::Int(3)
    );
    let keys_wrong = evaluate_text(
        "<expr>",
        "val ks: List<Int> = Map#keys(%[\"a\": 1 \"b\": 2])\nks",
    )
    .expect_err("String keys assigned to List<Int> should fail");
    assert!(keys_wrong.to_string().contains("type mismatch"));

    let union_mixed = evaluate_text("<expr>", "Set#union(%(1 2 3), %(\"a\" \"b\"))")
        .expect_err("union of differently-typed sets should fail");
    assert!(union_mixed.to_string().contains("type mismatch"));

    // Correct usage still type-checks.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "val s: Set<Int> = Set#union(%(1 2), %(2 3))\nSet#toList(s).size()",
        )
        .unwrap(),
        Value::Int(3)
    );
}

#[test]
fn match_resolves_constructors_against_the_scrutinee_enum() {
    // A user enum may reuse a prelude constructor name (`Ok`/`Err`,
    // `Some`/`None`). A match arm's constructor must resolve against the
    // scrutinee's own enum, not whichever enum the schema map happens to
    // iterate first — that made the same program compile or fail at random.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "enum Outcome<a, b> { case Ok(value: a); case Err(msg: b) }\n\
             def unwrap(r: Outcome<Int, String>): Int = r match { case Ok(v) => v; case Err(e) => 0 }\n\
             unwrap(Ok(42))",
        )
        .unwrap(),
        Value::Int(42)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "enum Maybe<a> { case Some(v: a); case None }\n\
             def f(m: Maybe<Int>): Int = m match { case Some(v) => v; case None => 0 }\n\
             f(Some(7))",
        )
        .unwrap(),
        Value::Int(7)
    );
}

#[test]
fn record_specs_are_covered_more_directly() {
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

    let constructor_error = evaluate_text(
        "<expr>",
        "record Person {\n  name: *\n  age: Int\n}\nval p = #Person(\"Hoge\", 1.0)",
    )
    .expect_err("record constructor mismatch should fail");
    assert!(constructor_error.to_string().contains("type mismatch"));

    let field_error = evaluate_text(
        "<expr>",
        "record Tuple<'a, 'b> {\n  _1: 'a\n  _2: 'b\n}\nval t = #Tuple(1, 2)\nval k: Double = t._1",
    )
    .expect_err("record field mismatch should fail");
    assert!(field_error.to_string().contains("type mismatch"));

    // A nominal record type annotation accepts the corresponding `#Name(...)`
    // constructor: the annotation `p: Point` and the constructed value
    // `#Point(...)` must unify. Both used to be different type
    // representations (`Named` vs `Record`), so the annotation was rejected
    // with `Point is not compatible with #Point`.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "record Point {\n  x: Int\n  y: Int\n}\ndef sum(p: Point): Int = p.x + p.y\nsum(#Point(3, 4))",
        )
        .unwrap(),
        Value::Int(7)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "record Point {\n  x: Int\n  y: Int\n}\nval q: Point = #Point(1, 2)\nq.y",
        )
        .unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "record Point {\n  x: Int\n  y: Int\n}\ndef mk(a: Int): Point = #Point(a, a + 1)\nmk(5).y",
        )
        .unwrap(),
        Value::Int(6)
    );

    // Structural-record width subtyping: a function whose parameter mentions
    // a subset of fields accepts a richer record (the documented
    // "looks for the fields it mentions and ignores the rest" behavior).
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def name_of(o: { name: String }): String = o.name\nname_of(record { name: \"Klassic\"; age: 7 })",
        )
        .unwrap(),
        Value::String("Klassic".to_string())
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "def add(r: { a: Int; b: Int }): Int = r.a + r.b\nadd(record { a: 1; b: 2; c: 3; d: 4 })",
        )
        .unwrap(),
        Value::Int(3)
    );
    // A missing required field is still rejected (narrowing is not allowed).
    let missing = evaluate_text(
        "<expr>",
        "def need_x(r: { x: Int }): Int = r.x\nneed_x(record { y: 2 })",
    )
    .expect_err("a record missing a required field should be rejected");
    assert!(
        missing
            .to_string()
            .contains("is not available on this record")
    );

    // A bare lowercase record type parameter (`record Box<a>`) is a type
    // variable, consistent with enums (`enum Option<a>`). It used to be
    // treated as a concrete type, so `#Box(5)` failed to unify; only the
    // `'a` form worked.
    assert_eq!(
        evaluate_text("<expr>", "record Box<a> {\n  v: a\n}\nval b = #Box(5)\nb.v",).unwrap(),
        Value::Int(5)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "record Pair<a, b> {\n  fst: a\n  snd: b\n}\nval p = #Pair(1, \"two\")\np.snd",
        )
        .unwrap(),
        Value::String("two".to_string())
    );
}

#[test]
fn dynamic_escape_hatch_specs_are_covered_more_directly() {
    assert_eq!(
        evaluate_text("<expr>", "val s: * = (100 :> *)\ns").unwrap(),
        Value::Int(100)
    );
    assert_eq!(
        evaluate_text("<expr>", "\"x + y = #{(10 + 20) :> *}\"").unwrap(),
        Value::String("x + y = 30".to_string())
    );
}

#[test]
fn runtime_error_helper_specs_are_covered_more_directly() {
    assert_eq!(
        evaluate_text("<expr>", "assert(2 == 1 + 1)").unwrap(),
        Value::Unit
    );
    assert_eq!(
        evaluate_text("<expr>", "assertResult(\"A\")(\"A\")").unwrap(),
        Value::Unit
    );

    let todo = evaluate_text(
        "<expr>",
        "def fact(n) = if(n < 2) ToDo() else n * fact(n - 1)\nfact(0)",
    )
    .expect_err("ToDo should fail at runtime");
    assert!(todo.to_string().contains("not implemented yet"));
}

#[test]
fn embedded_macro_peg_block_spec_is_supported() {
    assert_eq!(
        evaluate_text("<expr>", "rule {\n  S = \"a\";\n}\n1").unwrap(),
        Value::Int(1)
    );
}

#[test]
fn string_utils_specs_are_covered() {
    assert_eq!(
        evaluate_text("<expr>", "join([\"hello\", \"world\"], \", \")").unwrap(),
        Value::String("hello, world".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", "replaceAll(\"123-456-789\", \"[0-9]\", \"X\")").unwrap(),
        Value::String("XXX-XXX-XXX".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", "reverse(\"hello\")").unwrap(),
        Value::String("olleh".to_string())
    );
}

#[test]
fn list_function_specs_are_covered() {
    assert_eq!(
        evaluate_text("<expr>", "3 #cons (2 #cons (1 #cons []))").unwrap(),
        Value::List(vec![Value::Int(3), Value::Int(2), Value::Int(1)])
    );
    assert_eq!(
        evaluate_text("<expr>", "[1 2 3] map =>e + 1").unwrap(),
        Value::List(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
    );
}

#[test]
fn typeclass_simple_specs_are_covered() {
    let program = r#"
        typeclass Printable<'a> where {
          print: ('a) => String
        }

        instance Printable<Int> where {
          def print(x: Int): String = "Int=" + x
        }

        instance Printable<Boolean> where {
          def print(x: Boolean): String = if(x) "YES" else "NO"
        }

        val s1 = print(42)
        val s2 = print(true)
        val s3 = print(false)

        s1 + ", " + s2 + ", " + s3
    "#;
    assert_eq!(
        evaluate_text("<expr>", program).unwrap(),
        Value::String("Int=42, YES, NO".to_string())
    );
}

#[test]
fn typeclass_bare_type_param_specs_are_covered() {
    // A bare lowercase class type parameter (`typeclass Show<a>`) is a type
    // variable, consistent with enums/records/defs/extensions. It used to be
    // treated as a concrete type, so the method signature `(a) => String` did
    // not match the `Int` instance and `show(42)` failed.
    let program = r#"
        typeclass Show<a> where {
          show: (a) => String
        }

        instance Show<Int> where {
          def show(x: Int): String = "I" + x
        }

        instance Show<Boolean> where {
          def show(b: Boolean): String = if(b) "Y" else "N"
        }

        show(42) + ", " + show(true)
    "#;
    assert_eq!(
        evaluate_text("<expr>", program).unwrap(),
        Value::String("I42, Y".to_string())
    );
}

#[test]
fn instance_method_body_is_checked_against_its_return_type() {
    // An instance method whose body disagrees with its declared return type is
    // rejected at compile time instead of crashing at runtime. The body used
    // to be unchecked entirely.
    let wrong_return = evaluate_text(
        "<expr>",
        "typeclass Compute<'a> where { compute: ('a) => Int }\n\
         instance Compute<Boolean> where { def compute(x: Boolean): Int = \"not an int\" }\n\
         compute(true)",
    )
    .expect_err("an instance body that returns the wrong type should fail");
    assert!(
        wrong_return.to_string().contains("type mismatch"),
        "expected a type-mismatch error, got: {wrong_return}"
    );

    // A class method called inside an instance body dispatches through the
    // polymorphic class method (here `show` on a `String` field), so a
    // correct instance that references other instances still type-checks.
    let valid = evaluate_text(
        "<expr>",
        "typeclass Show<'a> where { show: ('a) => String }\n\
         instance Show<Int> where { def show(x: Int): String = \"i\" + x }\n\
         instance Show<String> where { def show(x: String): String = x }\n\
         record Person { name: String; age: Int }\n\
         instance Show<Person> where { def show(p: Person): String = show(p.name) + \"/\" + show(p.age) }\n\
         show(#Person(\"a\", 7))",
    )
    .unwrap();
    assert_eq!(valid, Value::String("a/i7".to_string()));
}

#[test]
fn instance_method_signature_is_checked_against_the_class() {
    // An instance method whose parameter type does not match the class
    // method instantiated at the instance type is rejected. `Compute<String>`
    // requires `compute: (String) => Int`, so a `(Int) => Int` method is a
    // type error instead of crashing at runtime when the String is used.
    let wrong_param = evaluate_text(
        "<expr>",
        "typeclass Compute<'a> where { compute: ('a) => Int }\n\
         instance Compute<String> where { def compute(x: Int): Int = x - 1 }\n\
         compute(\"hello\")",
    )
    .expect_err("an instance method with the wrong parameter type should fail");
    assert!(
        wrong_param.to_string().contains("type mismatch"),
        "expected a type-mismatch error, got: {wrong_param}"
    );

    // A multi-argument class method is checked position by position.
    let wrong_second = evaluate_text(
        "<expr>",
        "typeclass Eq<'a> where { equals: ('a, 'a) => Boolean }\n\
         instance Eq<Int> where { def equals(x: Int, y: String): Boolean = true }\n\
         equals(1, 2)",
    )
    .expect_err("a mismatched second parameter should fail");
    assert!(wrong_second.to_string().contains("type mismatch"));

    // A correct generic instance still type-checks.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "typeclass Show<'a> where { show: ('a) => String }\n\
             instance Show<Int> where { def show(x: Int): String = \"i\" }\n\
             instance Show<List<'a>> where Show<'a> { def show(xs: List<'a>): String = \"L\" }\n\
             show([1 2 3])",
        )
        .unwrap(),
        Value::String("L".to_string())
    );
}

#[test]
fn two_constraints_on_same_class_use_distinct_type_variables() {
    // `def pair<Show 'a, Show 'b>` has two constraints on the same class with
    // different type variables. The class method `show` must be usable at both
    // `'a` and `'b` without forcing them equal, so a mixed-type call is
    // accepted. The method declaration used to be overwritten by the last
    // constraint, unifying `'a` and `'b` and spuriously rejecting `pair(1, "x")`.
    let program = r#"
        typeclass Show<'a> where { show: ('a) => String }
        instance Show<Int> where { def show(x: Int): String = "i" }
        instance Show<String> where { def show(x: String): String = "s" }
        def pair<Show 'a, Show 'b>(x: 'a, y: 'b): String = show(x) + show(y)
        pair(1, "two")
    "#;
    assert_eq!(
        evaluate_text("<expr>", program).unwrap(),
        Value::String("is".to_string())
    );

    // A missing instance for either type variable is still rejected.
    let missing = evaluate_text(
        "<expr>",
        "typeclass Show<'a> where { show: ('a) => String }\n\
         instance Show<Int> where { def show(x: Int): String = \"i\" }\n\
         def pair<Show 'a, Show 'b>(x: 'a, y: 'b): String = show(x) + show(y)\n\
         pair(1, true)",
    )
    .expect_err("a missing instance for one constraint should fail");
    assert!(
        missing.to_string().contains("missing instance for Show"),
        "expected a missing-instance error, got: {missing}"
    );
}

#[test]
fn constrained_binding_indirect_reference_carries_constraint() {
    // Assigning a class-constrained generic function to a concrete function
    // type discharges its constraint at that type: `display` needs `Show`, so
    // binding it at `(Boolean) => String` reports the missing `Show<Boolean>`
    // instance instead of dropping the constraint and crashing at runtime.
    let no_instance = evaluate_text(
        "<expr>",
        "typeclass Show<'a> where { show: ('a) => String }\n\
         instance Show<Int> where { def show(x: Int): String = \"n:\" + x }\n\
         def display<Show 'a>(x: 'a): String = show(x)\n\
         val f: (Boolean) => String = display\n\
         f(true)",
    )
    .expect_err("a constrained binding bound at a type without an instance should fail");
    assert!(
        no_instance
            .to_string()
            .contains("missing instance for Show"),
        "expected a missing-instance error, got: {no_instance}"
    );

    // Binding it at a type that does have an instance still works.
    assert_eq!(
        evaluate_text(
            "<expr>",
            "typeclass Show<'a> where { show: ('a) => String }\n\
             instance Show<Int> where { def show(x: Int): String = \"n:\" + x }\n\
             def display<Show 'a>(x: 'a): String = show(x)\n\
             val f: (Int) => String = display\n\
             f(5)",
        )
        .unwrap(),
        Value::String("n:5".to_string())
    );

    // Storing the polymorphic constrained function in a `val` and calling it
    // later at a concrete type still works (the free constraint generalizes).
    assert_eq!(
        evaluate_text(
            "<expr>",
            "typeclass Show<'a> where { show: ('a) => String }\n\
             instance Show<Int> where { def show(x: Int): String = \"i\" + x }\n\
             def display<Show 'a>(x: 'a): String = show(x)\n\
             val g = display\n\
             g(5)",
        )
        .unwrap(),
        Value::String("i5".to_string())
    );
}

#[test]
fn higher_kinded_typeclass_specs_are_covered() {
    let program = r#"
        typeclass Functor<'f: * => *> where {
          map: (('a) => 'b, 'f<'a>) => 'f<'b>
        }

        instance Functor<List> where {
          def map(f: ('a) => 'b, xs: List<'a>): List<'b> = {
            xs.map(f)
          }
        }

        val numbers = [1, 2, 3]
        map((x) => x * 2, numbers)
    "#;

    assert_eq!(
        evaluate_text("<expr>", program).unwrap(),
        Value::List(vec![Value::Int(2), Value::Int(4), Value::Int(6)])
    );
}

#[test]
fn typeclass_specs_are_covered_more_directly() {
    let parse_typeclass = r#"
        typeclass Show<'a> where {
          show: ('a) => String
        }
        1
    "#;
    assert_eq!(
        evaluate_text("<expr>", parse_typeclass).unwrap(),
        Value::Int(1)
    );

    let parse_instance = r#"
        typeclass Show<'a> where {
          show: ('a) => String
        }

        instance Show<Int> where {
          def show(x: Int): String = "Int: " + x
        }
        2
    "#;
    assert_eq!(
        evaluate_text("<expr>", parse_instance).unwrap(),
        Value::Int(2)
    );

    let direct_method = r#"
        typeclass Show<'a> where {
          show: ('a) => String
        }

        instance Show<Int> where {
          def show(x: Int): String = "Int(" + x + ")"
        }

        instance Show<String> where {
          def show(x: String): String = "String(" + x + ")"
        }

        show(42)
    "#;
    assert_eq!(
        evaluate_text("<expr>", direct_method).unwrap(),
        Value::String("Int(42)".to_string())
    );

    let multi_method = r#"
        typeclass Eq<'a> where {
          equals: ('a, 'a) => Boolean
          notEquals: ('a, 'a) => Boolean
        }

        instance Eq<Int> where {
          def equals(x: Int, y: Int): Boolean = x == y
          def notEquals(x: Int, y: Int): Boolean = x != y
        }

        instance Eq<String> where {
          def equals(x: String, y: String): Boolean = x == y
          def notEquals(x: String, y: String): Boolean = x != y
        }

        equals(5, 5)
    "#;
    assert_eq!(
        evaluate_text("<expr>", multi_method).unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn typeclass_simple_specs_are_covered_more_directly() {
    let show_program = r#"
        typeclass Show<'a> where {
          show: ('a) => String
        }

        instance Show<Int> where {
          def show(x: Int): String = "number: " + x
        }

        val result = show(42)
        result
    "#;
    assert_eq!(
        evaluate_text("<expr>", show_program).unwrap(),
        Value::String("number: 42".to_string())
    );

    let multi_instance_program = r#"
        typeclass Show<'a> where {
          show: ('a) => String
        }

        instance Show<Int> where {
          def show(x: Int): String = "[" + x + "]"
        }

        instance Show<String> where {
          def show(x: String): String = "<<" + x + ">>"
        }

        val r1 = show(123)
        val r2 = show("hello")

        r1 + " and " + r2
    "#;
    assert_eq!(
        evaluate_text("<expr>", multi_instance_program).unwrap(),
        Value::String("[123] and <<hello>>".to_string())
    );

    let record_program = r#"
        typeclass Display<'a> where {
          display: ('a) => String
        }

        record Point {
          x: Int
          y: Int
        }

        instance Display<Point> where {
          def display(p: Point): String = "(" + p.x + "," + p.y + ")"
        }

        val p = #Point(3, 4)
        display(p)
    "#;
    assert_eq!(
        evaluate_text("<expr>", record_program).unwrap(),
        Value::String("(3,4)".to_string())
    );
}

#[test]
fn typeclass_usage_specs_are_covered_more_directly() {
    let methods_program = r#"
        typeclass Show<'a> where {
          show: ('a) => String
        }

        instance Show<Int> where {
          def show(x: Int): String = "Int: " + x
        }

        instance Show<String> where {
          def show(x: String): String = "Str: " + x
        }

        val result1 = show(42)
        val result2 = show("hello")

        assert(result1 == "Int: 42")
        assert(result2 == "Str: hello")

        "Type class methods work"
    "#;
    assert_eq!(
        evaluate_text("<expr>", methods_program).unwrap(),
        Value::String("Type class methods work".to_string())
    );

    let multi_instance_program = r#"
        typeclass Eq<'a> where {
          equals: ('a, 'a) => Boolean
        }

        instance Eq<Int> where {
          def equals(x: Int, y: Int): Boolean = x == y
        }

        instance Eq<String> where {
          def equals(x: String, y: String): Boolean = x == y
        }

        record Person {
          name: String
          age: Int
        }

        instance Eq<Person> where {
          def equals(p1: Person, p2: Person): Boolean =
            equals(p1.name, p2.name) && equals(p1.age, p2.age)
        }

        val p1 = #Person("Alice", 30)
        val p2 = #Person("Alice", 30)
        val p3 = #Person("Bob", 25)

        assert(equals(10, 10))
        assert(!equals(10, 20))
        assert(equals("foo", "foo"))
        assert(equals(p1, p2))
        assert(!equals(p1, p3))

        "Multiple instances work"
    "#;
    assert_eq!(
        evaluate_text("<expr>", multi_instance_program).unwrap(),
        Value::String("Multiple instances work".to_string())
    );

    let practical_program = r#"
        typeclass Serializable<'a> where {
          serialize: ('a) => String
          deserialize: (String) => 'a
        }

        instance Serializable<Int> where {
          def serialize(x: Int): String = x.toString()
          def deserialize(s: String): Int = int(double(1))
        }

        instance Serializable<List<Int>> where {
          def serialize(xs: List<Int>): String =
            "[" + join(map(xs)((x) => serialize(x)), ",") + "]"
          def deserialize(s: String): List<Int> =
            [1, 2, 3]
        }

        val nums = [10, 20, 30]
        val serialized = serialize(nums)
        assert(serialized == "[10,20,30]")

        "Serialization works"
    "#;
    assert_eq!(
        evaluate_text("<expr>", practical_program).unwrap(),
        Value::String("Serialization works".to_string())
    );
}

#[test]
fn higher_kinded_typeclass_specs_are_covered_more_directly() {
    let parse_program = r#"
        typeclass Functor<'f: * => *> where {
          map: (('a) => 'b, 'f<'a>) => 'f<'b>
        }

        typeclass Monad<'m: * => *> where {
          bind: ('m<'a>, ('a) => 'm<'b>) => 'm<'b>;
          unit: ('a) => 'm<'a>
        }

        1
    "#;
    assert_eq!(
        evaluate_text("<expr>", parse_program).unwrap(),
        Value::Int(1)
    );

    let instance_program = r#"
        typeclass Functor<'f: * => *> where {
          map: (('a) => 'b, 'f<'a>) => 'f<'b>
        }

        instance Functor<List> where {
          def map(f: ('a) => 'b, xs: List<'a>): List<'b> = {
            xs.map(f)
          }
        }

        2
    "#;
    assert_eq!(
        evaluate_text("<expr>", instance_program).unwrap(),
        Value::Int(2)
    );

    let resolve_program = r#"
        typeclass Functor<'f: * => *> where {
          map: (('a) => 'b, 'f<'a>) => 'f<'b>
        }

        instance Functor<List> where {
          def map(f: ('a) => 'b, xs: List<'a>): List<'b> = {
            xs.map(f)
          }
        }

        val numbers = [1, 2, 3]
        map((x) => x * 2, numbers)
    "#;
    assert_eq!(
        evaluate_text("<expr>", resolve_program).unwrap(),
        Value::List(vec![Value::Int(2), Value::Int(4), Value::Int(6)])
    );
}

#[test]
fn user_defined_module_persistence_is_covered_more_directly() {
    assert_eq!(
        evaluate_text("<expr>", "module persisted.demo\ndef inc(x) = x + 1").unwrap(),
        Value::Unit
    );
    assert_eq!(
        evaluate_text("<expr>", "import persisted.demo\ninc(41)").unwrap(),
        Value::Int(42)
    );

    let mut evaluator = Evaluator::new();
    assert_eq!(
        evaluator
            .evaluate_text("<repl>", "module repl.demo\ndef twice(x) = x + x")
            .unwrap(),
        Value::Unit
    );
    assert_eq!(
        evaluator
            .evaluate_text("<repl>", "import repl.demo\ntwice(21)")
            .unwrap(),
        Value::Int(42)
    );
}

#[test]
fn trust_flag_specs_are_covered_more_directly() {
    let denied = evaluate_text_with_config(
        "<expr>",
        "trust theorem foo(): { true } = assert(true)",
        EvaluatorConfig {
            deny_trust: true,
            warn_trust: false,
        },
    )
    .expect_err("deny-trust should reject trusted proofs");
    assert!(
        denied
            .to_string()
            .contains("trusted proof 'foo' is not allowed (level 1)")
    );
}

#[test]
fn pragmatic_file_processing_spec_is_covered() {
    let csv = unique_temp_path("data.csv");
    let out = unique_temp_path("out.txt");
    let csv = path_string(&csv);
    let out = path_string(&out);
    let program = format!(
        r#"
        val csvContent = "Name,Age,City\nAlice,30,NYC\nBob,25,LA\nCarol,35,Chicago"
        FileOutput#write("{csv}", csvContent)

        val lines = FileInput#lines("{csv}")
        val data = tail(lines)
        val results = map(data)((row) => {{
          val fields = split(row, ",")
          val name = head(fields)
          val age = head(tail(fields))
          val city = head(tail(tail(fields)))
          toUpperCase(name) + " is " + age + " years old and lives in " + city
        }})

        FileOutput#writeLines("{out}", results)
        assert(FileOutput#exists("{out}"))

        FileOutput#delete("{csv}")
        FileOutput#delete("{out}")

        "Processing completed"
    "#
    );

    assert_eq!(
        evaluate_text("<expr>", &program).unwrap(),
        Value::String("Processing completed".to_string())
    );
}

#[test]
fn module_import_specs_are_covered_more_directly() {
    assert_eq!(
        evaluate_text("<expr>", "module foo.bar\n1 + 1").unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text("<expr>", "module foo.bar;\n1 + 1").unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text("<expr>", "import Map\nsize(%[\"A\": 1, \"B\": 2])").unwrap(),
        Value::Int(2)
    );
    assert_eq!(
        evaluate_text("<expr>", "import Set\ncontains(%(1, 2, 3))(2)").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            "import Map as M\nimport Map.{size}\nsize(%[\"A\": 1])"
        )
        .unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        evaluate_text("<expr>", "import Map as M\nM#size(%[\"A\": 1])").unwrap(),
        Value::Int(1)
    );

    assert_eq!(
        evaluate_text("<expr>", "module m\ndef twice(x) = x + x").unwrap(),
        Value::Unit
    );
    assert_eq!(
        evaluate_text("<expr>", "import m\ntwice(21)").unwrap(),
        Value::Int(42)
    );

    assert_eq!(
        evaluate_text("<expr>", "module user.util\ndef inc(x) = x + 1").unwrap(),
        Value::Unit
    );
    assert_eq!(
        evaluate_text("<expr>", "import user.util\ninc(41)").unwrap(),
        Value::Int(42)
    );
    assert_eq!(
        evaluate_text("<expr>", "user.util#inc(41)").unwrap(),
        Value::Int(42)
    );
}

#[test]
fn file_input_specs_are_covered_more_directly() {
    let path = unique_temp_path("hello.txt");
    std::fs::write(&path, "Hello, World!").expect("temp input file should be writable");
    let path = path_string(&path);

    let open_all =
        format!("\"{path}\" FileInput#open {{(stream) =>\n  FileInput#readAll(stream)\n}}");
    assert_eq!(
        evaluate_text("<expr>", &open_all).unwrap(),
        Value::String("Hello, World!".to_string())
    );

    let open_lines =
        format!("\"{path}\" FileInput#open {{(stream) =>\n  FileInput#readLines(stream)\n}}");
    assert_eq!(
        evaluate_text("<expr>", &open_lines).unwrap(),
        Value::List(vec![Value::String("Hello, World!".to_string())])
    );

    let all = format!("FileInput#all(\"{path}\")");
    assert_eq!(
        evaluate_text("<expr>", &all).unwrap(),
        Value::String("Hello, World!".to_string())
    );

    let lines = format!("FileInput#lines(\"{path}\")");
    assert_eq!(
        evaluate_text("<expr>", &lines).unwrap(),
        Value::List(vec![Value::String("Hello, World!".to_string())])
    );
}

#[test]
fn file_output_specs_are_covered_more_directly() {
    let write_path = unique_temp_path("write.txt");
    let write_path_str = path_string(&write_path);
    let write_program = format!(r#"FileOutput#write("{write_path_str}", "Hello, World!")"#);
    let _ = evaluate_text("<expr>", &write_program).unwrap();
    assert_eq!(
        std::fs::read_to_string(&write_path).unwrap(),
        "Hello, World!"
    );

    let append_path = unique_temp_path("append.txt");
    let append_path_str = path_string(&append_path);
    let append_program = format!(
        "FileOutput#write(\"{append_path_str}\", \"Line 1\")\nFileOutput#append(\"{append_path_str}\", \"\\nLine 2\")"
    );
    let _ = evaluate_text("<expr>", &append_program).unwrap();
    assert_eq!(
        std::fs::read_to_string(&append_path).unwrap(),
        "Line 1\nLine 2"
    );

    let exists_path = unique_temp_path("exists.txt");
    let exists_path_str = path_string(&exists_path);
    let exists_before = format!(r#"FileOutput#exists("{exists_path_str}")"#);
    assert_eq!(
        evaluate_text("<expr>", &exists_before).unwrap(),
        Value::Bool(false)
    );
    std::fs::write(&exists_path, "test").unwrap();
    let exists_after = format!(r#"FileOutput#exists("{exists_path_str}")"#);
    assert_eq!(
        evaluate_text("<expr>", &exists_after).unwrap(),
        Value::Bool(true)
    );

    let delete_path = unique_temp_path("delete.txt");
    std::fs::write(&delete_path, "test").unwrap();
    let delete_path_str = path_string(&delete_path);
    let delete_program = format!(r#"FileOutput#delete("{delete_path_str}")"#);
    let _ = evaluate_text("<expr>", &delete_program).unwrap();
    assert!(!delete_path.exists());

    let lines_path = unique_temp_path("lines.txt");
    let lines_path_str = path_string(&lines_path);
    let lines_program = format!(
        "val lines = [\"Line 1\", \"Line 2\", \"Line 3\"]\nFileOutput#writeLines(\"{lines_path_str}\", lines)"
    );
    let _ = evaluate_text("<expr>", &lines_program).unwrap();
    assert_eq!(
        std::fs::read_to_string(&lines_path).unwrap().trim(),
        "Line 1\nLine 2\nLine 3"
    );

    let io_path = unique_temp_path("io.txt");
    let io_path_str = path_string(&io_path);
    let io_program = format!(
        "FileOutput#write(\"{io_path_str}\", \"Test content\")\nval content = FileInput#all(\"{io_path_str}\")\nassert(content == \"Test content\")"
    );
    assert_eq!(evaluate_text("<expr>", &io_program).unwrap(), Value::Unit);
}

#[test]
fn dir_specs_are_covered_more_directly() {
    let current = evaluate_text("<expr>", "Dir#current()").unwrap();
    let home = evaluate_text("<expr>", "Dir#home()").unwrap();
    let temp = evaluate_text("<expr>", "Dir#temp()").unwrap();
    assert!(matches!(current, Value::String(_)));
    assert!(matches!(home, Value::String(_)));
    assert!(matches!(temp, Value::String(_)));

    let command_line_args = evaluate_text("<expr>", "CommandLine#args()").unwrap();
    assert!(matches!(
        command_line_args,
        Value::List(values) if values.iter().all(|value| matches!(value, Value::String(_)))
    ));
    let unqualified_args = evaluate_text("<expr>", "args()").unwrap();
    assert!(matches!(
        unqualified_args,
        Value::List(values) if values.iter().all(|value| matches!(value, Value::String(_)))
    ));

    let base = unique_temp_path("dir-base");
    let base_str = path_string(&base);
    let exists_before = format!(r#"Dir#exists("{base_str}")"#);
    assert_eq!(
        evaluate_text("<expr>", &exists_before).unwrap(),
        Value::Bool(false)
    );
    let mkdir_program = format!(r#"Dir#mkdir("{base_str}")"#);
    let _ = evaluate_text("<expr>", &mkdir_program).unwrap();
    assert_eq!(
        evaluate_text("<expr>", &format!(r#"Dir#isDirectory("{base_str}")"#)).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_text("<expr>", &format!(r#"Dir#isFile("{base_str}")"#)).unwrap(),
        Value::Bool(false)
    );

    let nested = base.join("child").join("grandchild");
    let nested_str = path_string(&nested);
    let mkdirs_program = format!(r#"Dir#mkdirs("{nested_str}")"#);
    let _ = evaluate_text("<expr>", &mkdirs_program).unwrap();
    assert!(nested.exists());

    let list_dir = unique_temp_path("list-dir");
    std::fs::create_dir_all(&list_dir).unwrap();
    std::fs::write(list_dir.join("file1.txt"), "content1").unwrap();
    std::fs::write(list_dir.join("file2.txt"), "content2").unwrap();
    std::fs::create_dir_all(list_dir.join("subdir")).unwrap();
    let list_program = format!(r#"Dir#list("{}")"#, path_string(&list_dir));
    let listed = evaluate_text("<expr>", &list_program).unwrap();
    match listed {
        Value::List(values) => {
            assert!(values.contains(&Value::String("file1.txt".to_string())));
            assert!(values.contains(&Value::String("file2.txt".to_string())));
            assert!(values.contains(&Value::String("subdir".to_string())));
        }
        other => panic!("unexpected list result: {other:?}"),
    }

    let source = unique_temp_path("copy-source.txt");
    let target = unique_temp_path("copy-target.txt");
    std::fs::write(&source, "test content").unwrap();
    let copy_program = format!(
        r#"Dir#copy("{}", "{}")"#,
        path_string(&source),
        path_string(&target)
    );
    let _ = evaluate_text("<expr>", &copy_program).unwrap();
    assert_eq!(std::fs::read_to_string(&target).unwrap(), "test content");

    let move_source = unique_temp_path("move-source.txt");
    let move_target = unique_temp_path("move-target.txt");
    std::fs::write(&move_source, "move content").unwrap();
    let move_program = format!(
        r#"Dir#move("{}", "{}")"#,
        path_string(&move_source),
        path_string(&move_target)
    );
    let _ = evaluate_text("<expr>", &move_program).unwrap();
    assert!(!move_source.exists());
    assert_eq!(
        std::fs::read_to_string(&move_target).unwrap(),
        "move content"
    );
}

#[test]
fn string_utils_specs_are_covered_more_directly() {
    assert_eq!(
        evaluate_text("<expr>", r#"split("hello,world,test", ",")"#).unwrap(),
        Value::List(vec![
            Value::String("hello".to_string()),
            Value::String("world".to_string()),
            Value::String("test".to_string()),
        ])
    );
    assert_eq!(
        evaluate_text("<expr>", r#"split("a b c", " ")"#).unwrap(),
        Value::List(vec![
            Value::String("a".to_string()),
            Value::String("b".to_string()),
            Value::String("c".to_string()),
        ])
    );
    assert_eq!(
        evaluate_text("<expr>", r#"join(["a", "b", "c"], "-")"#).unwrap(),
        Value::String("a-b-c".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", r#"trimLeft("  hello  ")"#).unwrap(),
        Value::String("hello  ".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", r#"replace("hello world", "world", "klassic")"#).unwrap(),
        Value::String("hello klassic".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", r#"toUpperCase("hello")"#).unwrap(),
        Value::String("HELLO".to_string())
    );
    assert_eq!(
        evaluate_text("<expr>", r#"startsWith("hello world", "hello")"#).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_text("<expr>", r#"indexOf("hello world", "world")"#).unwrap(),
        Value::Int(6)
    );
    assert_eq!(
        evaluate_text("<expr>", r#"repeat("ab", 3)"#).unwrap(),
        Value::String("ababab".to_string())
    );
    assert_eq!(
        evaluate_text(
            "<expr>",
            r#"val text = "  Hello, World!  "
val words = split(trim(text), ", ")
val upper = map(words)((w) => toUpperCase(w))
join(upper, " - ")"#,
        )
        .unwrap(),
        Value::String("HELLO - WORLD!".to_string())
    );
}
