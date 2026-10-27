//! Shared nested-output program for Linux, Windows and AArch64 test emitters.
pub(crate) const SOURCE: &str = r#"
record Pair { left: String; right: String }
enum Pick<a> { case Both(left: a, right: a); case Missing }
enum Mono { case Wrap(left: String, right: String); case Stop }
def fresh(s: String): String = s + "!"
println([fresh("a"), fresh("b")])
println(#Pair(fresh("c"), fresh("d")))
println(Both(fresh("e"), fresh("f")))
println(Wrap(fresh("g"), fresh("h")))
println(%(fresh("i"), fresh("j")))
println(%["k!": fresh("l")])
println([[fresh("m"), fresh("n")], [fresh("o")]])
println([#Pair(fresh("p"), fresh("q")), #Pair(fresh("r"), fresh("s"))])
println(fresh("tail"))
"#;
pub(crate) const EXPECTED: &[u8] = b"[a!, b!]\n#Pair(c!, d!)\nBoth(e!, f!)\nWrap(g!, h!)\n%(i!, j!)\n%[k!: l!]\n[[m!, n!], [o!]]\n[#Pair(p!, q!), #Pair(r!, s!)]\ntail!\n";

pub(crate) fn lowered() -> crate::DesugaredEnums {
    crate::desugar_enums(
        klassic_syntax::parse_inline_expression("<nested-output>", SOURCE).unwrap(),
    )
}
