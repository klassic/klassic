package com.github.klassic.macro_peg

import com.github.klassic.{Parser => KlassicParser}

object Runner {
  def main(args: Array[String]): Unit = {
    evalGrammar(
      """S = A !.; A = "1" A "1" / "+" A / "=";""",
      Seq("1+1=11","11+1=111","11+1=1", "11+11+11=111111")
    )
    evalGrammar(
      """S = P("") !.; P(r) = "a" P("a" r) / "b" P("b" r) / r;""",
      Seq("a", "b", "aa", "bb", "ab", "abba", "abbb")
    )
    evalGrammar(
      """
     |S = APPLY2(ALTER, "a", "b") !.; ALTER(x, y) = Debug(x / y) (x / y); APPLY2(F, x, y) = F(x, y) ;
     """.stripMargin,
      Seq("a", "b", "c")
    )
    evalGrammar(
       """
      |S = (Plus0("") / Mul0("")) !.;
      |// the number of occurence of '1 represents a natural number.
      |// a+b=c
      |Plus0(Left) = Plus1(Left, "") / &(Left "1") Plus0(Left "1");
      |
      |Plus1(Left, Right)
      |  = &(Left "+" Right "=") Plus2(Left, Right)
      |  / &(Left "+" Right "1") Plus1(Left, Right "1");
      |
      |Plus2(Left, Right)
      |  = Left "+" Right "=" Left Right;
      |
      |// check a*b=c
      |Mul0(Left)
      |  = &(Left "*") Mul1(Left, "", "")
      |  / &(Left "1") Mul0(Left "1");
      |
      |Mul1(Left, Right, Prod)
      |  = &(Left "*" Right "=") Mul2(Left, Right, Prod)
      |  / &(Left "*" Right "1") Mul1(Left, Right "1", Prod Left);
      |
      |Mul2(Left, Right, Prod)
      |  = Left "*" Right "=" Prod;
      |
      """.stripMargin,
      Seq("1+1=11", "111+11=11111", "111+1=11111",  "111*11=111111", "11*111=111111", "1*111=1")
    )
    evalGrammar(
      """
    |S = Modifiers(!"", "") !.;
    |Modifiers(AlreadyLooked, Scope) = (!AlreadyLooked) (
    |    &(Scope) Token("public") Modifiers(AlreadyLooked / "public", "public")
    |  / &(Scope) Token("protected") Modifiers(AlreadyLooked / "protected", "protected")
    |  / &(Scope) Token("private") Modifiers(AlreadyLooked / "private", "private")
    |  / Token("static") Modifiers(AlreadyLooked / "static", Scope)
    |  / Token("final") Modifiers(AlreadyLooked / "final", Scope)
    |  / ""
    |);
    |Token(t) = t Spacing;
    |Spacing = " "*;
    """.stripMargin,
      Seq("public static final", "public public", "public static public", "final static public", "final final", "public private", "protected public", "public static")
    )
    evalGrammar(
        """
      |S = ReadRight("") !.;
      |// the number of occurence of '1 represents a natural number.
      |// a-b=c
      |// Essentially, this checks a=b+c.
      |ReadRight(Right)
      |  = &("1"* "-" Right "1") ReadRight(Right "1")
      |  / &("1"* "-" Right "=") ReadDiff(Right, "");
      |
      |ReadDiff(Right, Diff)
      |  = &("1"* "-" Right "=" Diff "1") ReadDiff(Right, Diff "1")
      |  / &("1"* "-" Right "=" Diff !.) Check(Right, Diff);
      |
      |Check(Right, Diff)
      |  = Right Diff "-" Right "=" Diff;
      """.stripMargin,
      Seq(
        "11-1=1", "1-1=", "111-11=1", // should match
        "111-1=1",  "111-1=111", "1-11=" // should not match
      )
    )

    evalGrammar(
        """
      |S = ReadLeft("", "") !.;
      |// the number of occurence of '1 represents a natural number.
      |// |Seq| is the length of a sequence Seq.
      |// ^ is exponent operator
      |// ReadLeft("", "") checks input is a correct expression a^b=c.
      |
      |// Read a.
      |// LeftAsOnes is a sequence of "1" where |LeftAsOnes| = |a|.
      |// LeftAsDots is a sequence of . where |LeftAsDots| = |a|.
      |ReadLeft(LeftAsOnes, LeftAsDots)
      |  = &(LeftAsOnes "1") ReadLeft(LeftAsOnes "1", LeftAsDots .)
      |  / &(LeftAsOnes "^") ComputePadding(LeftAsOnes, LeftAsDots, "");
      |
      |// Compute Padding which is a sequene of .
      |// where |Padding| + |LeftAsDots| = |Input|
      |ComputePadding(LeftAsOnes, LeftAsDots, Padding)
      |  = &(Padding LeftAsDots .) ComputePadding(LeftAsOnes, LeftAsDots, Padding .)
      |  / &(Padding LeftAsDots !.) ReadRight(LeftAsOnes, Padding, "", "1");
      |
      |// Read b.
      |// Exp = a^Right.
      |ReadRight(Left, Padding, Right, Exp)
      |  = &(Left "^" Right "1") Multiply(Left, Padding, Right "1", Exp, "", "")
      |  / &(Left "^" Right "=") Check(Left, Right, Exp);
      |
      |// Compute Left * OldExp.
      |// This adds OldExp Left times into Exp.
      |// I is a loop counter.
      |Multiply(Left, Padding, Right, OldExp, Exp, I)
      |  = &(Padding I .) Multiply(Left, Padding, Right, OldExp, Exp OldExp, I .)
      |  / &(Padding I !.) ReadRight(Left, Padding, Right, Exp);
      |
      |// Check whole input.
      |Check(Left, Right, Exp)
      |  = Left "^" Right "=" Exp;
      """.stripMargin,
      Seq(
        "11^111=11111111", "11^=1", "1^11=1", "^11=", // should match
        "11^111=1111111",  "11^111=111111111" // should not match
      )
    )

    evalGrammar(
      """S = [a-zA-Z_][a-zA-Z0-9_]*;""",
      Seq("hoge", "foo", "hoge1", "foo1", "1foo", "2hoge", "123")
    )
  }

  /**
    * Try to recognize *inputs* by *Evaluator* generated by *source*.
    * @param source a String that represents Macro PEG
    * @param inputs input Strings
    * @return sequence of results
    */
  def evalGrammar(source: String, inputs: Seq[String], strategy: EvaluationStrategy = EvaluationStrategy.CallByName): Seq[EvaluationResult] = {
    val parser = new KlassicParser
    val grammar = parser.parsePeg(source)
    val evaluator = Evaluator(grammar, strategy)
    for(input <- inputs) yield evaluator.evaluate(input, 'S)
  }
}
