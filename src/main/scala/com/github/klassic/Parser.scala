package com.github.klassic

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{CharSequenceReader, Position, Reader}
import com.github.klassic.AST._
import com.github.klassic.Type._

/**
 * @author Kota Mizushima
 */
class Parser extends RegexParsers {
  override def skipWhitespace = false
  private def not[T](p: => Parser[T], msg: String): Parser[Unit] = {
    not(p) | failure(msg)
  }
  private def and[T](p: => Parser[T], msg: String): Parser[Unit] = {
    not(not(p)) | failure(msg)
  }
  lazy val % : Parser[SourceLocation] = Parser{reader => Success(reader.pos, reader)}.map{position =>
    SourceLocation(position.line, position.column)
  }
  lazy val EOF: Parser[String] = not(elem(".", (ch: Char) => ch != CharSequenceReader.EofCh), "EOF Expected") ^^ {_.toString}
  lazy val LINEFEED : Parser[String] = ("\r\n" | "\r" | "\n")
  lazy val SEMICOLON: Parser[String] = ";"
  lazy val ANY: Parser[String] = elem(".", (ch: Char) => ch != CharSequenceReader.EofCh) ^^ {_.toString}

  lazy val SPACING: Parser[String] = (COMMENT | "\r\n" | "\r" | "\n" | " " | "\t" | "\b" | "\f").* ^^ {_.mkString}
  lazy val SPACING_WITHOUT_LF: Parser[String] = (COMMENT | "\t" | " " | "\b" | "\f").* ^^ {_.mkString}
  lazy val TERMINATOR: Parser[String] = (LINEFEED | SEMICOLON | EOF) <~ SPACING
  lazy val SEPARATOR: Parser[String] = (LINEFEED | COMMA | EOF | SPACING_WITHOUT_LF) <~ SPACING

  lazy val BLOCK_COMMENT: Parser[Any] = (
    "/*" ~ (not("*/") ~ (BLOCK_COMMENT | ANY)).* ~ "*/"
  )
  lazy val LINE_COMMENT: Parser[Any] = (
    "//" ~ (not(LINEFEED) ~ ANY).* ~ LINEFEED
  )
  lazy val COMMENT: Parser[Any] = BLOCK_COMMENT | LINE_COMMENT

  def CL[T](parser: Parser[T]): Parser[T] = parser <~ SPACING
  def token(parser: Parser[String]): Parser[String] = parser <~ SPACING_WITHOUT_LF
  def unescape(input: String): String = {
    val builder = new java.lang.StringBuilder
    val length = input.length
    var i = 0
    while(i < length - 1) {
      (input.charAt(i), input.charAt(i + 1)) match {
        case ('\\', 'r') => builder.append('\r'); i += 2
        case ('\\', 'n') => builder.append('\n'); i += 2
        case ('\\', 'b') => builder.append('\b'); i += 2
        case ('\\', 'f') => builder.append('\f'); i += 2
        case ('\\', 't') => builder.append('\t'); i += 2
        case ('\\', '\\') => builder.append('\\'); i += 2
        case (ch, _) => builder.append(ch); i += 1
      }
    }
    if(i == length - 1) {
      builder.append(input.charAt(i))
    }
    new String(builder)
  }

  lazy val LT      : Parser[String] = token("<")
  lazy val GT      : Parser[String] = token(">")
  lazy val LTE     : Parser[String] = token("<=")
  lazy val GTE     : Parser[String] = token(">=")
  lazy val MAP_OPEN: Parser[String] = token("%[")
  lazy val SET_OPEN: Parser[String] = token("%(")
  lazy val PLUS    : Parser[String] = token("+")
  lazy val MINUS   : Parser[String] = token("-")
  lazy val ASTER   : Parser[String] = token("*")
  lazy val SLASH   : Parser[String] = token("/")
  lazy val LPAREN  : Parser[String] = token("(")
  lazy val RPAREN  : Parser[String] = token(")")
  lazy val LBRACE  : Parser[String] = token("{")
  lazy val RBRACE  : Parser[String] = token("}")
  lazy val LBRACKET: Parser[String] = token("[")
  lazy val RBRACKET: Parser[String] = token("]")
  lazy val SHARP   : Parser[String] = token("#")
  lazy val IF      : Parser[String] = token("if")
  lazy val ELSE    : Parser[String] = token("else")
  lazy val WHILE   : Parser[String] = token("while")
  lazy val FOREACH : Parser[String] = token("foreach")
  lazy val IMPORT  : Parser[String] = token("import")
  lazy val VARIANT : Parser[String] = token("variant")
  lazy val TRUE    : Parser[String] = token("true")
  lazy val FALSE   : Parser[String] = token("false")
  lazy val IN      : Parser[String] = token("in")
  lazy val COMMA   : Parser[String] = token(",")
  lazy val DOT     : Parser[String] = token(".")
  lazy val CLASS   : Parser[String] = token("class")
  lazy val RECORD  : Parser[String] = token("record")
  lazy val DEF     : Parser[String] = token("def")
  lazy val MUTABLE : Parser[String] = token("mutable")
  lazy val CLEANUP : Parser[String] = token("cleanup")
  lazy val VAL     : Parser[String] = token("val")
  lazy val EQ      : Parser[String] = token("=")
  lazy val PLUSEQ  : Parser[String] = token("+=")
  lazy val MINUSEQ : Parser[String] = token("-=")
  lazy val ASTEREQ : Parser[String] = token("*=")
  lazy val SLASHEQ : Parser[String] = token("/=")
  lazy val COLONGT : Parser[String] = token(":>")
  lazy val EQEQ    : Parser[String] = token("==")
  lazy val ARROW1  : Parser[String] = token("=>")
  lazy val ARROW2  : Parser[String] = token("->")
  lazy val COLON   : Parser[String] = token(":")
  lazy val NEW     : Parser[String] = token("new")
  lazy val QUES    : Parser[String] = token("?")
  lazy val AMP2    : Parser[String] = token("&&")
  lazy val BAR2    : Parser[String] = token("||")
  lazy val BAR     : Parser[String] = token("|")
  lazy val KEYWORDS: Set[String]     = Set(
    "<", ">", "<=", ">=", "+", "-", "*", "/", "{", "}", "[", "]", ":", "?",
    "if", "else", "while", "foreach", "import", "cleanup", "true", "false", "in", ",", ".",
    "class", "def", "val", "mutable", "=", "==", "=>", "new", "&&", "||"
  )

  lazy val typeAnnotation: Parser[Type] = COLON ~> typeDescription

  lazy val castType: Parser[Type] = typeDescription

  def isBuiltinType(name: String): Boolean = name match {
    case "Byte" => true
    case "Short" => true
    case "Int" => true
    case "Long" => true
    case "Float" => true
    case "Double" => true
    case "Boolean" => true
    case "Unit" => true
    case _ => false
  }

  lazy val typeDescription: Parser[Type] = (
    ((CL(LPAREN) ~> repsep(typeDescription, CL(COMMA)) <~ CL(RPAREN)) <~ CL(ARROW1)) ~ typeDescription ^^ { case args ~ returnType => FunctionType(args, returnType)}
  | sident.^?{ case s if !isBuiltinType(s) => s} ~ (CL(LT) ~> repsep(typeDescription, CL(COMMA)) <~ CL(GT)).? ^^ {
      case name ~ Some(args) => TypeConstructor(name, args)
      case name ~ None => TypeConstructor(name, Nil)
    }
  | qident ^^ {id => TypeVariable(id) }
  | token("Byte") ^^ {_ => ByteType}
  | token("Short")  ^^ {_ => ShortType}
  | token("Int")  ^^ {_ => IntType}
  | token("Long")  ^^ {_ => LongType}
  | token("Float") ^^ {_ => FloatType}
  | token("Double")  ^^ {_ => DoubleType}
  | token("Boolean")  ^^ {_ => BooleanType}
  | token("Unit") ^^ {_ => UnitType}
  | token("*") ^^ {_ => DynamicType}
  )

  lazy val program: Parser[Program] = (SPACING ~> %) ~ repsep(`import`, TERMINATOR) ~ repsep(record, TERMINATOR) ~ (lines <~ opt(TERMINATOR)) ^^ {
    case location ~ imports ~ records ~ block => Program(location, imports, records, block)
  }

  lazy val `import`: Parser[Import] = (% <~ CL(IMPORT)) ~ fqcn ^^ { case location ~ fqcn =>
    val fragments = fqcn.split(".")
    Import(location, fragments(fragments.length - 1), fqcn)
  }

  lazy val record: Parser[RecordDeclaration] = for {
    location <- %
    _ <- CL(RECORD)
    name <- sident
    ts <- opt(LT ~> rep1sep(typeAnnotation, CL(COMMA)) <~ CL(GT))
    _ <- CL(LBRACE)
    members <- (sident ~ CL(typeAnnotation) ^^ { case n ~ t => (n, t) }).*
    _ <- CL(RBRACE)
  } yield RecordDeclaration(location, name, ts.getOrElse(Nil), members)

  //lines ::= line {TERMINATOR expr} [TERMINATOR]
  lazy val lines: Parser[Block] = SPACING ~> (% ~ repsep(line, TERMINATOR)) <~ opt(TERMINATOR) ^^ { case location ~ expressions =>
      Block(location, expressions)
  }

  lazy val variantDeclaration: Parser[VariantDeclaration] = for {
    location <- %
    _ <- CL(VARIANT)
    name <- sident
    ts <- opt(LT ~> rep1sep(typeAnnotation, CL(COMMA)) <~ GT <~ EQ)
    cs <- dataConstructor.*
  } yield VariantDeclaration(location, name, ts.getOrElse(Nil), cs)

  lazy val dataConstructor: Parser[DataConstructor] = (
    (BAR ~> sident) ~ opt(LPAREN ~> rep1sep((sident <~ CL(COLON)) ~ typeAnnotation ^^ { case i ~ t => FormalParameter(i, t)}, CL(COMMA)) <~ RPAREN) ^^ {
      case name ~ Some(params) => DataConstructor(name, params)
      case name ~ None => DataConstructor(name, Nil)
    }
  )

  //line ::= expression | valDeclaration | functionDefinition
  lazy val line: Parser[AST] = variantDeclaration | expression | valDeclaration | functionDefinition

  //expression ::= assignment | infix | ifExpression | whileEpression | foreachExpression
  lazy val expression: Parser[AST] = assignment | infix | ifExpression | whileExpression | foreachExpression

  //ifExpression ::= "if" "(" expression ")" expression "else" expression
  lazy val ifExpression: Parser[AST] = (% <~ CL(IF) <~ CL(LPAREN)) ~ expression ~ CL(RPAREN) ~ expression ~ CL(ELSE) ~ expression ^^ {
    case location ~ condition ~ _ ~ positive ~ _ ~ negative => IfExpression(location, condition, positive, negative)
  }

  //whileExpression ::= "while" "(" expression ")" expression
  lazy val whileExpression: Parser[AST] = (% <~ CL(WHILE) <~ CL(LPAREN)) ~ expression ~ (CL(RPAREN) ~> expression) ^^ {
    case location ~ condition ~  body => WhileExpression(location, condition, body)
  }

  //foreachExpression ::= "foreach" "(" ident "in" expression ")" expression
  lazy val foreachExpression: Parser[AST] = (% <~ CL(FOREACH) <~ CL(LPAREN)) ~ (CL(ident) <~ CL(IN)) ~ (expression <~ CL(RPAREN)) ~ expression ^^ {
    case location ~ variable ~ collection ~ body => ForeachExpression(location, variable.name, collection, body)
  }

  lazy val infix: Parser[AST] = chainl1(logical,
      (% ~ CL(operator)) ^^ { case location ~ op => (lhs: AST, rhs: AST) => FunctionCall(location, FunctionCall(location, Id(location, op), List(lhs)), List(rhs))}
    | (% ~ CL(selector)) ^^ { case location ~ sl => (lhs: AST, rhs: AST) => FunctionCall(location, FunctionCall(location, sl, List(lhs)), List(rhs)) }
  )

  lazy val logical: Parser[AST] = chainl1(conditional,
    (% <~ CL(AMP2)) ^^ {location => (lhs: AST, rhs: AST) => BinaryExpression(location, Operator.AND2, lhs, rhs)} |
    (% <~ CL(BAR2)) ^^ {location => (lhs: AST, rhs: AST) => BinaryExpression(location, Operator.BAR2, lhs, rhs)}
  )

  //conditional ::= add {"<" add | ">" add | "<=" add | ">=" add}
  lazy val conditional: Parser[AST] = chainl1(add,
    (% <~ CL(EQEQ)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.EQUAL, left, right)} |
    (% <~ CL(LTE)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.LESS_OR_EQUAL, left, right)} |
    (% <~ CL(GTE)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.GREATER_EQUAL, left, right)} |
    (% <~ CL(LT)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.LESS_THAN, left, right)} |
    (% <~ CL(GT)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.GREATER_THAN, left, right)}
  )


  //add ::= term {"+" term | "-" term}
  lazy val add: Parser[AST] = chainl1(term,
    (% <~ CL(PLUS)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.ADD, left, right)}|
    (% <~ CL(MINUS)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.SUBTRACT, left, right)})

  //term ::= factor {"*" factor | "/" factor}
  lazy val term : Parser[AST] = chainl1(unary,
    (% <~ CL(ASTER)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.MULTIPLY, left, right)}|
    (% <~ CL(SLASH)) ^^ {location => (left:AST, right:AST) => BinaryExpression(location, Operator.DIVIDE, left, right)})

  lazy val unary: Parser[AST] = (
    % ~ CL(MINUS) ~ unary ^^ { case location ~ _ ~ operand => MinusOp(location, operand) }
  | % ~ CL(PLUS) ~ unary ^^ { case location ~ _ ~ operand => PlusOp(location, operand) }
  | invocation
  )

  lazy val invocation: Parser[AST] = % ~ recordAccess ~ ((CL(DOT) ~> ident) ~ opt(CL(LPAREN) ~> repsep(expression, CL(COMMA)) <~ RPAREN)).* ^^ {
    case location ~ self ~ Nil =>
      self
    case location ~ self ~ npList  =>
      npList.foldLeft(self){case (self, name ~ params) => MethodCall(location, self, name.name, params.getOrElse(Nil))}
  }

  lazy val recordAccess: Parser[AST] = % ~ application ~ ((CL(ARROW1) ~> (% ~ ident))).* ^^ {
    case location ~ self ~ names =>
      val ns = names.map{ case l ~ n => (l, n.name)}
      ns.foldLeft(self) { case (e, (l, n)) =>
        RecordAccess(l, e, n)
      }
  }

  lazy val application: Parser[AST] = % ~ pipelinable ~ (CL(LPAREN) ~> repsep(CL(expression), CL(COMMA)) <~ (SPACING <~ RPAREN)).* ^^ {
    case location ~ f ~ xs => {
       xs.foldLeft(f){ case (e, x) => FunctionCall(e.location, e, x) }
    }
  }

  lazy val pipelinable: Parser[AST] = % ~ castable ~ opt(CL(ARROW2) ~> ident) ^^ {
    case location ~ self ~ None =>
      self
    case location ~ self ~ Some(name) =>
      FunctionCall(location, name, List(self))
  }

  lazy val castable: Parser[AST] = primary ~ opt((% <~ CL(COLONGT)) ~ CL(castType)) ^^ {
    case target ~ Some((location ~ castType)) => Casting(location, target, castType)
    case target ~ None => target
  }

  //primary ::= selector | booleanLiteral | ident | floatLiteral | integerLiteral | mapLiteral | stringLiteral | listLiteral | |setLiteral | newRecord | newObject | functionLiteral | "(" expression ")" | "{" lines "}" | hereDocument
  lazy val primary: Parser[AST] = selector | booleanLiteral | ident | floatLiteral | integerLiteral | mapLiteral | stringLiteral | listLiteral | setLiteral | newRecord | newObject | functionLiteral | CL(LPAREN) ~>expression<~ RPAREN | CL(LBRACE) ~>lines<~ RBRACE | hereDocument

  //integerLiteral ::= ["1"-"9"] {"0"-"9"}
  lazy val integerLiteral : Parser[AST] = (% ~ """[1-9][0-9]*|0""".r ~ opt("BY" ^^ { _ => ByteSuffix } | "L" ^^ { _ => LongSuffix} | "S" ^^ { _ => ShortSuffix }) ^^ {
    case location ~ value ~ None => IntNode(location, value.toLong.toInt)
    case location ~ value ~ Some(LongSuffix) => LongNode(location, value.toLong)
    case location ~ value ~ Some(ShortSuffix) => ShortNode(location, value.toShort)
    case location ~ value ~ Some(ByteSuffix) => ByteNode(location, value.toByte)
  }) <~ SPACING_WITHOUT_LF

  lazy val floatLiteral: Parser[AST]= ((% ~ "([1-9][0-9]*|0)\\.[0-9]*".r ~ opt("F" ^^ { _ => FloatSuffix })) ^^ {
    case location ~ value ~ None => DoubleNode(location, value.toDouble)
    case location ~ value ~ Some(FloatSuffix) => FloatNode(location, value.toFloat)
  }) <~ SPACING_WITHOUT_LF

  lazy val booleanLiteral: Parser[AST] = % ~ (TRUE ^^ { _ => true }| FALSE ^^ { _ => false}) ^^ {
    case location ~  true => BooleanNode(location, true)
    case location ~  false => BooleanNode(location, false)
  }

  //stringLiteral ::= "\"" ((?!")(\[rntfb"'\\]|[^\\]))* "\""
  lazy val stringLiteral : Parser[AST] =
    ("\"" ~>
      (% ~ """((?!("|#\{))(\\[rntfb"'\\]|[^\\]))+""".r ^^ {case location ~ in =>
        StringNode(location, unescape(in))
      } | "#{" ~> expression <~ "}"
      ).*
    <~ "\"" ^^ { values =>
    values.foldLeft(StringNode(NoLocation, ""):AST) { (node, content) => BinaryExpression(content.location, Operator.ADD, node, content) }
  }) <~ SPACING_WITHOUT_LF

  lazy val listLiteral: Parser[AST] = % ~ (CL(LBRACKET) ~> (repsep(CL(expression), SEPARATOR) <~ opt(SEPARATOR)) <~ RBRACKET) ^^ {
    case location ~ contents => ListLiteral(location, contents)
  }

  lazy val setLiteral: Parser[AST] = % ~ (CL(SET_OPEN) ~> (repsep(CL(expression), SEPARATOR) <~ opt(SEPARATOR)) <~ RPAREN) ^^ {
    case location ~ contents => SetLiteral(location, contents)
  }

  lazy val mapLiteral: Parser[AST] = % ~ (CL(MAP_OPEN) ~> (repsep(CL(expression ~ COLON ~ expression), SEPARATOR) <~ opt(SEPARATOR)) <~ RBRACKET) ^^ {
    case location ~ contents => MapLiteral(location, contents.map {case k ~ colon ~ v => (k, v)})
  }

  lazy val fqcn: Parser[String] = (ident ~ (CL(DOT) ~ ident).*) ^^ {
    case id ~ ids => ids.foldLeft(id.name){ case (a, d ~ e) => a + d + e.name}
  }

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

  lazy val hereDocument: Parser[StringNode] = ("""<<[a-zA-Z_][a-zA-Z0-9_]*""".r >> { t =>
    val tag = t.substring(2)
    Parser{in =>
      val Success(temp, rest) = oneLine(in)

      val line = new CharSequenceReader(temp, 0)
      hereDocumentBody(tag).apply(rest) match {
        case Success(value, next) =>
          val source = cat(line, next)
          Success(StringNode(NoLocation, value), source)
        case Failure(msg, next) => Failure(msg, cat(line, next))
        case Error(msg, next) => Error(msg, cat(line, next))
      }
    }
  }) <~ SPACING_WITHOUT_LF

  def hereDocumentBody(beginTag: String): Parser[String] = oneLine >> {line =>
    if(beginTag == line.trim) "" else hereDocumentBody(beginTag) ^^ {result =>
      line + result
    }
  }

  lazy val component: Parser[String] = """[A-Za-z_][a-zA-Z0-9_]*""".r

  lazy val ident :Parser[Id] = (% ~ component ^? {
    case r@(_ ~ n) if !KEYWORDS(n) => r
  } ^^ {case location ~ name => Id(location, name)}) <~ SPACING_WITHOUT_LF

  def selector: Parser[Selector] = (% ~ component ~ "#" ~ component ^? {
    case r@(_ ~ m ~ _ ~ n) if (!KEYWORDS(m)) && (!KEYWORDS(n)) => r
  } ^^ {case location ~ m ~ _ ~ n => Selector(location, m, n)}) <~ SPACING_WITHOUT_LF

  lazy val qident:Parser[String] = ("""'[A-Za-z_][a-zA-Z0-9_]*""".r^? {
    case  n if !KEYWORDS(n) => n
  }) <~ SPACING_WITHOUT_LF

  lazy val sident:Parser[String] = ("""[A-Za-z_][a-zA-Z0-9_]*""".r^? {
    case  n if !KEYWORDS(n) => n
  }) <~ SPACING_WITHOUT_LF

  lazy val operator:Parser[String] = ("""#[A-Za-z_][a-zA-Z0-9_]*""".r^? {
    case  n if !KEYWORDS(n.substring(1)) => n.substring(1)
  }) <~ SPACING_WITHOUT_LF

  lazy val assignment: Parser[Assignment] = ident ~ CL(PLUSEQ | MINUSEQ | ASTEREQ | SLASHEQ | EQ) ~ expression ^^ {
    case v ~ "=" ~ value => SimpleAssignment(v.location, v.name, value)
    case v ~ "+=" ~ value => PlusAssignment(v.location, v.name, value)
    case v ~ "-=" ~ value => MinusAssignment(v.location, v.name, value)
    case v ~ "*=" ~ value => MultiplicationAssignment(v.location, v.name, value)
    case v ~ "/=" ~ value => DivisionAssignment(v.location, v.name, value)
    case _ ~ op ~ _ => sys.error(s"unknown assignment operator ${op}")
  }

  // valDeclaration ::= "val" ident "=" expression
  lazy val valDeclaration:Parser[ValDeclaration] = ((% ~ CL(MUTABLE ^^ {_ => false } | VAL ^^ {_ => true})) ~ ident ~ opt(typeAnnotation) <~ CL(EQ)) ~ expression ^^ {
    case location ~  immutable ~ valName ~ optionalType ~ value => ValDeclaration(location, valName.name, optionalType, value, immutable)
  }

  // anonnymousFunction ::= "(" [param {"," param}] ")" "=>" expression
  lazy val functionLiteral: Parser[AST] = % ~ opt(CL(LPAREN) ~> repsep(ident ~ opt(typeAnnotation), CL(COMMA)) <~ CL(RPAREN)) ~ (opt(typeAnnotation) <~ CL(ARROW1)) ~ expression ^^ {
    case location ~ Some(params) ~ optionalType ~ body =>
      Lambda(
        location,
        params.map {
          case name ~ Some(description) => FormalParameterOptional(name.name, Some(description))
          case name ~ None => FormalParameterOptional(name.name, None)
        },
        optionalType,
        body
      )
    case location ~ None ~ optionalType ~ body => Lambda(location, List(), optionalType, body)
  }

  // newObject ::= "new" fqcn "(" [param {"," param} ")"
  lazy val newObject: Parser[AST] = (% <~ CL(NEW)) ~ fqcn ~ opt(CL(LPAREN) ~> repsep(expression, CL(COMMA)) <~ RPAREN) ^^ {
    case location ~ className ~ Some(params) => NewObject(location, className, params)
    case location ~ className ~ None => NewObject(location, className, List())
  }

  // newRecord ::= "new" "#" fqcn "(" [param {"," param} ")"
  lazy val newRecord: Parser[AST] = ((% <~ CL(NEW)) <~ CL(SHARP)) ~ sident ~ opt(CL(LPAREN) ~> repsep(expression, CL(COMMA)) <~ RPAREN) ^^ {
    case location ~ recordName ~ Some(params) => NewRecord(location, recordName, params)
    case location ~ recordName ~ None => NewRecord(location, recordName, List())
  }

  // functionDefinition ::= "def" ident  ["(" [param {"," param]] ")"] "=" expression
  lazy val functionDefinition: Parser[FunctionDefinition] =
    (% <~ CL(DEF)) ~ ident ~ opt(CL(LPAREN) ~>repsep(ident ~ opt(typeAnnotation), CL(COMMA)) <~ CL(RPAREN)) ~ (opt(typeAnnotation) <~ CL(EQ)) ~ expression ~ opt(CL(CLEANUP) ~> expression) ^^ {
    case location ~ functionName ~ params ~ optionalType ~ body ~ cleanup =>
      val ps = params match {
        case Some(xs) =>
          xs.map{
            case name ~ Some(annotation) => FormalParameterOptional(name.name, Some(annotation))
            case name ~ None => FormalParameterOptional(name.name, None)
          }
        case None => Nil
      }
      FunctionDefinition(
        location,
        functionName.name,
        Lambda(body.location, ps, optionalType, body),
        cleanup
      )
  }

  def parseExpression(input: String): ParseResult[AST] = parseAll(lines, input)

  def parse(input: String): ParseResult[Program] = parseAll(program, input)
}
