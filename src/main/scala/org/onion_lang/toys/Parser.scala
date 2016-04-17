package org.onion_lang
package toys
import scala.util.parsing.combinator.RegexParsers
import util.parsing.input.{Reader, CharSequenceReader}

/**
 * @author Kota Mizushima
 */
class Parser extends RegexParsers {
  override def skipWhitespace = false
  lazy val SPACING: Parser[String] = """\s*""".r
  lazy val SEMICOLON: Parser[String] = ";" <~ SPACING
  lazy val LT: Parser[String] = "<" <~ SPACING
  lazy val GT: Parser[String] = "<" <~ SPACING
  lazy val PLUS: Parser[String] = "+" <~ SPACING
  lazy val MINUS: Parser[String] = "-" <~ SPACING
  lazy val ASTER: Parser[String] = "*" <~ SPACING
  lazy val SLASH: Parser[String] = "/" <~ SPACING
  lazy val LPAREN: Parser[String] = "(" <~ SPACING
  lazy val RPAREN: Parser[String] = ")" <~ SPACING
  lazy val LBRACE: Parser[String] = "{" <~ SPACING
  lazy val RBRACE: Parser[String] = "}" <~ SPACING
  lazy val IF: Parser[String] = "if" <~ SPACING
  lazy val ELSE: Parser[String] = "else" <~ SPACING
  lazy val COMMA: Parser[String] = "," <~ SPACING
  lazy val PRINTLN: Parser[String] = "println" <~ SPACING
  lazy val DEF: Parser[String] = "def" <~ SPACING
  lazy val VAL: Parser[String] = "val" <~ SPACING
  lazy val EQ: Parser[String] = "=" <~ SPACING
  lazy val ARROW: Parser[String] = "=>" <~ SPACING

  //lines ::= expr {";" expr} [";"]
  def lines: Parser[AST] = repsep(line, SEMICOLON)<~opt(SEMICOLON)^^Block

  def line: Parser[AST] = expr | val_declaration | funcDef
  //expr ::= cond | if | printLine
  def expr: Parser[AST] = assignment|condOp|ifExpr|printLine
  //if ::= "if" "(" expr ")" expr "else" expr
  def ifExpr: Parser[AST] = IF ~ LPAREN ~> expr ~ RPAREN ~ expr ~ ELSE ~ expr^^{
    case cond~_~pos~_~neg => IfExpr(cond, pos, neg)
  }
  //cond ::= add {"<" add}
  def condOp: Parser[AST] = chainl1(add,
    LT ^^{op => (left:AST, right:AST) => LessOp(left, right)})
  //add ::= term {"+" term | "-" term}.
  def add: Parser[AST] = chainl1(term,
    PLUS ^^ {op => (left:AST, right:AST) => AddOp(left, right)}|
    MINUS ^^ {op => (left:AST, right:AST) => SubOp(left, right)})
  //term ::= factor {"*" factor | "/" factor}
  def term : Parser[AST] = chainl1(funcCall,
    ASTER ^^ {op => (left:AST, right:AST) => MulOp(left, right)}|
    SLASH ^^ {op => (left:AST, right:AST) => DivOp(left, right)})

  def funcCall: Parser[AST] = factor ~ opt(LPAREN ~> repsep(expr, COMMA) <~ RPAREN)^^{
    case fac~param =>{
        param match{
          case Some(p) => FuncCall(fac, p)
          case None => fac
        }
    }
  }

  //factor ::= intLiteral | stringLiteral | "(" expr ")" | "{" lines "}"
  def factor: Parser[AST] = intLiteral | stringLiteral | ident | anonFun | LPAREN ~>expr<~ RPAREN | LBRACE ~>lines<~ RBRACE | hereDocument | hereExpression

  //intLiteral ::= ["1"-"9"] {"0"-"9"}
  def intLiteral : Parser[AST] = ("""[1-9][0-9]*|0""".r^^{ value => IntVal(value.toInt)}) <~ SPACING

  //stringLiteral ::= "\"" ((?!")(\[rnfb"'\\]|[^\\]))* "\""
  def stringLiteral : Parser[AST] = ("\""~> ("""((?!("|#\{))(\[rnfb"'\\]|[^\\]))+""".r ^^ StringVal | "#{" ~> expr <~ "}").*  <~ "\"" ^^ {values =>
    values.foldLeft(StringVal(""):AST) {(ast, content) => AddOp(ast, content) }
  }) <~ SPACING

  def rebuild(a: Reader[Char], newSource: String, newOffset: Int): Reader[Char] = new Reader[Char] {
    def atEnd = a.atEnd
    def first = a.first
    def pos = a.pos
    def rest = rebuild(a.rest, newSource, offset + 1)
    override def source = newSource
    override def offset = newOffset
  }

  def cat(a: Reader[Char], b: Reader[Char]): Reader[Char] = {
    val aSource = a.source + b.source.subSequence(b.offset, b.source.length()).toString
    if(a.atEnd) {
      rebuild(b, aSource, a.offset)
    } else {
      new Reader[Char] {
        private lazy val result = cat(a.rest, b)
        def atEnd = a.atEnd
        def first = a.first
        def pos = a.pos
        def rest = result
        override def source = aSource
        override def offset = a.offset
      }
    }
  }

  lazy val oneLine: Parser[String] = regex(""".*(\r\n|\r|\n|$)""".r)

  lazy val hereDocument: Parser[StringVal] = ("""<<[a-zA-Z_][a-zA-Z0-9_]*""".r >> {t =>
    val tag = t.substring(2)
    Parser{in =>
      val Success(temp, rest) = oneLine(in)

      val line = new CharSequenceReader(temp, 0)
      hereDocumentBody(tag).apply(rest) match {
        case Success(value, next) =>
          val source = cat(line, next)
          Success(StringVal(value), source)
        case Failure(msg, next) => Failure(msg, cat(line, next))
        case Error(msg, next) => Error(msg, cat(line, next))
      }
    }
  }) <~ SPACING

  def hereDocumentBody(beginTag: String): Parser[String] = oneLine >> {line =>
    if(beginTag == line.trim) "" else hereDocumentBody(beginTag) ^^ {result =>
      line + result
    }
  }

  lazy val hereExpression: Parser[AST] = ("""<<\$[a-zA-Z_][a-zA-Z0-9_]*""".r >> {t =>
    val tag = t.substring(3)
    Parser{in =>
      val Success(temp, rest) = oneLine(in)

      val line = new CharSequenceReader(temp, 0)
      hereDocumentBody(tag).apply(rest) match {
        case Success(value, next) =>
          val Success(ast, _) = lines(new CharSequenceReader(value, 0))
          val source = cat(line, next)
          Success(ast, source)
        case Failure(msg, next) => Failure(msg, cat(line, next))
        case Error(msg, next) => Error(msg, cat(line, next))
      }
    }
  }) <~ SPACING

  def ident :Parser[Ident] = ("""[A-Za-z_][a-zA-Z0-9]*""".r^? {
    case n if n != "if" && n!= "val" && n!= "println" && n != "def" => n
  } ^^ Ident) <~ SPACING

  def assignment: Parser[Assignment] = (ident <~ EQ) ~ expr ^^ {
    case v ~ value => Assignment(v.name, value)
  }

  // val_declaration ::= "val" ident "=" expr
  def val_declaration:Parser[ValDeclaration] = (VAL ~> ident <~ EQ) ~ expr ^^ {
    case v ~ value => ValDeclaration(v.name, value)
  }
  // printLine ::= "printLn" "(" expr ")"
  def printLine: Parser[AST] = PRINTLN ~> (LPAREN ~> expr <~ RPAREN) ^^PrintLine

  // anonFun ::= "(" [param {"," param}] ")" "=>" expr
  def anonFun:Parser[AST] = ((LPAREN ~> repsep(ident, COMMA) <~ RPAREN) <~ ARROW) ~ expr ^^ {
    case params ~ proc => Func(params.map{_.name}, proc)
  }

  // funcDef ::= "def" ident  ["(" [param {"," param]] ")"] "=" expr
  def funcDef:Parser[FuncDef] = DEF ~> ident ~ opt(LPAREN ~>repsep(ident, COMMA) <~ RPAREN) ~ EQ ~ expr ^^ {
    case v~params~_~proc => {
        val p = params match {
          case Some(pr) => pr
          case None => Nil
        }
        FuncDef(v.name, Func(p.map{_.name}, proc))
    }
  }

  def parse(str:String) = parseAll(lines, str)
}
