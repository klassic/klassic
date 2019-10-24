package com.github.klassic.macro_peg

import java.io._

import com.github.klassic.macro_peg.Ast._
import com.github.kmizu.scomb._
/**
  * This object provides a parser that parses strings in Macro PEG and translates
  * them into ASTs of Macro PEG (which is like PEGs).
  * @author Kota Mizushima
  *
  */
object Parser {

  /**
   * This exception is thrown in the case of a parsing failure
 *
   * @param pos the position where the parsing failed
   * @param msg error message
   */
  case class ParseException(pos: Ast.Position, msg: String) extends Exception(pos.line + ", " + pos.column + ":" + msg)
  
  trait Fragment extends SCombinator[Any] {
    implicit class RichParse[A](self: Parser[A]) {
      def <~[B](rhs: Parser[B]): Parser[A] = self << rhs
      def ~>[B](rhs: Parser[B]): Parser[B] = self >> rhs
    }
    private def chr(c: Char): Parser[Char] = any.filter(_ == c,s"expected ${c}")
    private def crange(f: Char, t: Char): Parser[Char] = set(f to t).map{_.charAt(0)}
    private def cset(cs: Char*): Parser[Char] = set(cs).map{_.charAt(0)}
    private val escape: Map[Char, Char] = Map(
      'n' -> '\n', 'r' -> '\r', 't' -> '\t', 'f' -> '\f'
    )

    def root: Parser[Any]

    lazy val GRAMMAR: Parser[Grammar] = rule((loc <~ Spacing) ~ Definition.* <~ EndOfFile) ^^ {
      case pos ~ rules => Grammar(Position(pos.line, pos.column), rules)
    }

    lazy val Definition: Parser[Rule] = rule(Ident  ~ ((LPAREN ~> Arg.repeat1By(COMMA) <~ RPAREN).? <~ EQ) ~ (Expression <~ SEMI_COLON).commit) ^^ {
      case name ~ argsOpt ~ body =>
        Rule(name.pos, name.name, body, argsOpt.getOrElse(List()).map(_._1.name))
    }

    lazy val Arg: Parser[(Identifier, Option[Type])] = rule(Ident ~ (COLON ~> TypeTree).?) ^^ { case id ~ tpe => (id, tpe)}

    lazy val TypeTree: Parser[Type] = rule {
      RuleTypeTree | SimpleTypeTree
    }

    lazy val RuleTypeTree: Parser[RuleType] = rule {
      (OPEN ~> (SimpleTypeTree.repeat1By(COMMA) <~ CLOSE) ~ (loc <~ ARROW) ~ SimpleTypeTree) ^^ { case paramTypes ~ pos ~ resultType => RuleType(Position(pos.line, pos.column), paramTypes, resultType) }
    }

    lazy val SimpleTypeTree: Parser[SimpleType] = rule {
      loc <~ QUESTION ^^ { case pos => SimpleType(Position(pos.line, pos.column)) }
    }
    
    lazy val Expression: Parser[Expression] = rule(Sequencable.repeat1By(SLASH | BAR) ^^ { ns =>
      val x :: xs = ns; xs.foldLeft(x){(a, y) => Alternation(y.pos, a, y)}
    })
    lazy val Sequencable: Parser[Expression] = rule(Prefix.+ ^^ { ns =>
      val x :: xs = ns; xs.foldLeft(x){(a, y) => Sequence(y.pos, a, y)}
    })
    lazy val Prefix: Parser[Expression]     = rule(
      (loc <~ AND) ~ Suffix ^^ { case pos ~ e => AndPredicate(Position(pos.line, pos.column), e) }
    | (loc <~ NOT) ~ Suffix ^^ { case pos ~ e => NotPredicate(Position(pos.line, pos.column), e) }
    | Suffix
    )
    lazy val Suffix: Parser[Expression]     = rule(
      loc ~ Primary <~ QUESTION ^^ { case pos ~ e => Optional(Position(pos.line, pos.column), e) }
    | loc ~ Primary <~ STAR ^^ { case pos ~ e => Repeat0(Position(pos.line, pos.column), e) }
    | loc ~ Primary <~ PLUS ^^ { case pos ~ e => Repeat1(Position(pos.line, pos.column), e) }
    | Primary
    )
    lazy val Primary: Parser[Expression]    = rule(
      (loc <~ Debug) ~ (LPAREN ~> Expression <~ RPAREN) ^^ { case loc ~ body => Ast.Debug(Position(loc.line, loc.column), body)}
    | IdentifierWithoutSpace ~ (LPAREN ~> Expression.repeat0By(COMMA) <~ RPAREN) ^^ { case name ~ params => Ast.Call(Position(name.pos.line, name.pos.column), name.name, params) }
    | Ident
    | CLASS
    | (OPEN ~> (Ident.repeat0By(COMMA) ~ (loc <~ ARROW) ~ Expression) <~ CLOSE) ^^ { case ids ~ loc ~ body => Function(Position(loc.line, loc.column), ids.map(_.name), body) }
    | OPEN ~> Expression <~ CLOSE
    | loc <~ DOT ^^ { case pos => Wildcard(Position(pos.line, pos.column)) }
    | loc <~ chr('_') ^^ { case pos => StringLiteral(Position(pos.line, pos.column), "") }
    | Literal
    )
    lazy val loc: Parser[Location] = %
    lazy val IdentifierWithoutSpace: Parser[Identifier] = rule(loc ~ IdentStart ~ IdentCont.* ^^ {
      case pos ~ s ~ c => Identifier(Position(pos.line, pos.column), Symbol("" + s + c.foldLeft("")(_ + _)))
    })
    lazy val Ident: Parser[Identifier] = rule(IdentifierWithoutSpace <~ Spacing)
    lazy val IdentStart: Parser[Char] = rule(crange('a','z') | crange('A','Z') | chr('_'))
    lazy val IdentCont: Parser[Char] = rule(IdentStart | crange('0','9'))
    lazy val Literal: Parser[StringLiteral] = rule(loc ~ (chr('\"') ~> CHAR.* <~ chr('\"')) <~ Spacing) ^^ {
      case pos ~ cs => StringLiteral(Position(pos.line, pos.column), cs.mkString)
    }
    lazy val CLASS: Parser[CharClass] = rule {
      (loc <~ chr('[')) ~ chr('^').? ~ ((not(chr(']')) ~> Range).* <~ chr(']') ~> Spacing) ^^ {
        //negative character class
        case (pos ~ Some(_) ~ rs) => CharClass(Position(pos.line, pos.column), false, rs)
        //positive character class
        case (pos ~ None ~ rs) => CharClass(Position(pos.line, pos.column), true, rs)
      }
    }
    lazy val Range: Parser[CharClassElement] = rule(
      CHAR ~ chr('-') ~ CHAR ^^ { case f ~ _ ~ t => CharRange(f, t) }
    | CHAR ^^ { case c => OneChar(c) }
    )
    private val META_CHARS = List('"','\\')
    lazy val META: Parser[Char] = cset(META_CHARS:_*)
    lazy val HEX: Parser[Char] = crange('0','9') | crange('a', 'f')
    lazy val CHAR: Parser[Char] = ( 
      chr('\\') ~> cset('n','r','t','f') ^^ { case c => escape(c) }
    | chr('\\') ~> chr('u') ~> (HEX ~ HEX ~ HEX ~ HEX) ^^ {
        case u1 ~ u2 ~ u3 ~ u4 => Integer.parseInt("" + u1 + u2 + u3 + u4, 16).toChar
      }
    | chr('\\') ~ META ^^ { case _ ~ c => c }
    | chr('\\') ~ crange('0','2') ~ crange('0','7') ~ crange('0','7') ^^ { 
        case _ ~ a ~ b ~ c => Integer.parseInt("" + a + b + c, 8).toChar
      }
    | chr('\\') ~ crange('0','7') ~ crange('0','7').? ^^ {
        case _ ~ a ~ Some(b) => Integer.parseInt("" + a + b, 8).toChar
        case _ ~ a ~ _ => Integer.parseInt("" + a, 8).toChar
      }
    | not(META) ~> any ^^ { case c => c}
    )
    lazy val Debug = string("Debug") <~ Spacing
    lazy val LPAREN = chr('(') <~ Spacing
    lazy val RPAREN = chr(')') <~ Spacing
    lazy val LBRACKET = chr('[') <~ Spacing
    lazy val RBRACKET = chr(']') <~ Spacing
    lazy val COMMA = chr(',') <~ Spacing
    lazy val LT = chr('<') <~ Spacing
    lazy val GT = chr('>') <~ Spacing
    lazy val COLON = chr(':') <~ Spacing
    lazy val SEMI_COLON = chr(';') <~ Spacing
    lazy val EQ = chr('=') <~ Spacing
    lazy val SLASH = chr('/') <~ Spacing
    lazy val BAR = chr('|') <~ Spacing
    lazy val AND = chr('&') <~ Spacing
    lazy val NOT = chr('!') <~ Spacing
    lazy val QUESTION = chr('?') <~ Spacing
    lazy val STAR = chr('*') <~ Spacing
    lazy val PLUS = chr('+') <~ Spacing
    lazy val OPEN = chr('(') <~ Spacing
    lazy val CLOSE = chr(')') <~ Spacing
    lazy val DOT = chr('.') <~ Spacing
    lazy val ARROW = chr('-') <~ chr('>') <~ Spacing
    lazy val Spacing = (Space | Comment).*
    lazy val Comment = (
      chr('/') ~ chr('/') ~ (not(EndOfLine) ~ any).* ~ EndOfLine
    )
    lazy val Space = chr(' ') | chr('\t') | EndOfLine
    lazy val EndOfLine = chr('\r') ~ chr('\n') | chr('\n') | chr('\r')
    lazy val EndOfFile = not(any)
  }

  object CoreParser extends Fragment {
    override def root: Parser[Grammar] = GRAMMAR
  }

  /**
   * Parses a pattern from `content` and returns the `Grammar` instance, which is the parse result.
 *
   * @param fileName
   * @param content
   * @return `Grammar` instance
   */
  def parse(fileName: String, content: java.io.Reader): Grammar = {
    val input = Iterator.continually(content.read()).takeWhile(_ != -1).map(_.toChar).mkString
    CoreParser.parse(input) match {
      case Result.Success(node) => node.asInstanceOf[Grammar]
      case Result.Failure(location, message) =>
        throw ParseException(Position(location.line, location.column), message)
    }
  }

  /**
   * Parses a `pattern` and returns the `Grammar` instance, which is the parse result.
 *
   * @param pattern input string
   * @return `Grammar` instance
   */
  def parse(pattern: String): Grammar = {
    parse("", new StringReader(pattern))
  }

  def main(args: Array[String]): Unit = {
    val g = parse(args(0), new FileReader(args(0)))
    println(g)
  }
}
