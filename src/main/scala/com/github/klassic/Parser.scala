package com.github.klassic


import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{CharSequenceReader, Position, Reader}
import com.github.klassic.AST._
import com.github.klassic.Type._
import com.github.kmizu.scomb
import com.github.kmizu.scomb.{Result, SCombinator}

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class Parser extends Processor[String, Program] {
  private object KlassicParsers extends SCombinator[Program] {
    def publicLocations: mutable.Map[Int, scomb.Location] = locations

    implicit def stringToParser(literal: String): Parser[String] = $(literal)

    implicit def regexToParser(literal: Regex): Parser[String] = regularExpression(literal)

    def %% : Parser[SourceLocation] = % ^^ {l =>
      SourceLocation(l.line, l.column)
    }

    def commit[T](parser: Parser[T]): Parser[T] = parser.commit

    lazy val LINEFEED: Parser[String] = ("\r\n" | "\r" | "\n")

    lazy val SEMICOLON: Parser[String] = ";"

    lazy val ANY: Parser[String] = any ^^ {
      _.toString
    }

    lazy val SPACING: Parser[String] = rule {
      (COMMENT | "\r\n" | "\r" | "\n" | " " | "\t" | "\b" | "\f").* ^^ {
        _.mkString
      }
    }

    lazy val SPACING_WITHOUT_LF: Parser[String] = rule {
      (COMMENT | "\t" | " " | "\b" | "\f").* ^^ {
        _.mkString
      }
    }

    lazy val TERMINATOR: Parser[String] = rule {
      (LINEFEED | SEMICOLON | EOF) << SPACING
    }

    lazy val SEPARATOR: Parser[String] = rule {
      (LINEFEED | COMMA | EOF | SPACING_WITHOUT_LF) << SPACING
    }

    lazy val BLOCK_COMMENT: Parser[Any] = rule {
      "/*" ~ (not("*/") ~ (BLOCK_COMMENT | ANY)).* ~ "*/"
    }

    lazy val LINE_COMMENT: Parser[Any] = rule {
      "//" ~ (not(LINEFEED) ~ ANY).* ~ LINEFEED
    }

    lazy val COMMENT: Parser[Any] = rule{
      BLOCK_COMMENT | LINE_COMMENT
    }

    def CL[T](parser: Parser[T]): Parser[T] = parser << SPACING

    override def token (parser: String): Parser[String] = parser << SPACING_WITHOUT_LF

    def unescape(input: String): String = {
      val builder = new java.lang.StringBuilder
      val length = input.length
      var i = 0
      while (i < length - 1) {
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
      if (i == length - 1) {
        builder.append(input.charAt(i))
      }
      new String(builder)
    }

    lazy val LT: Parser[String] = token("<")
    lazy val GT: Parser[String] = token(">")
    lazy val LTE: Parser[String] = token("<=")
    lazy val GTE: Parser[String] = token(">=")
    lazy val MAP_OPEN: Parser[String] = token("%[")
    lazy val SET_OPEN: Parser[String] = token("%(")
    lazy val PLUS: Parser[String] = token("+")
    lazy val MINUS: Parser[String] = token("-")
    lazy val ASTER: Parser[String] = token("*")
    lazy val SLASH: Parser[String] = token("/")
    lazy val LPAREN: Parser[String] = token("(")
    lazy val RPAREN: Parser[String] = token(")")
    lazy val LBRACE: Parser[String] = token("{")
    lazy val RBRACE: Parser[String] = token("}")
    lazy val LBRACKET: Parser[String] = token("[")
    lazy val RBRACKET: Parser[String] = token("]")
    lazy val SHARP: Parser[String] = token("#")
    lazy val IF: Parser[String] = token("if")
    lazy val ELSE: Parser[String] = token("else")
    lazy val WHILE: Parser[String] = token("while")
    lazy val FOREACH: Parser[String] = token("foreach")
    lazy val IMPORT: Parser[String] = token("import")
    lazy val VARIANT: Parser[String] = token("variant")
    lazy val TRUE: Parser[String] = token("true")
    lazy val FALSE: Parser[String] = token("false")
    lazy val IN: Parser[String] = token("in")
    lazy val COMMA: Parser[String] = token(",")
    lazy val DOT: Parser[String] = token(".")
    lazy val CLASS: Parser[String] = token("class")
    lazy val RECORD: Parser[String] = token("record")
    lazy val DEF: Parser[String] = token("def")
    lazy val MUTABLE: Parser[String] = token("mutable")
    lazy val CLEANUP: Parser[String] = token("cleanup")
    lazy val VAL: Parser[String] = token("val")
    lazy val EQ: Parser[String] = token("=")
    lazy val BEGIN_MSTR: Parser[String] = token("<<<")
    lazy val END_MSTR: Parser[String] = token(">>>")
    lazy val PLUSEQ: Parser[String] = token("+=")
    lazy val MINUSEQ: Parser[String] = token("-=")
    lazy val ASTEREQ: Parser[String] = token("*=")
    lazy val SLASHEQ: Parser[String] = token("/=")
    lazy val COLONGT: Parser[String] = token(":>")
    lazy val EQEQ: Parser[String] = token("==")
    lazy val ARROW1: Parser[String] = token("=>")
    lazy val ARROW2: Parser[String] = token("->")
    lazy val COLON: Parser[String] = token(":")
    lazy val NEW: Parser[String] = token("new")
    lazy val QUES: Parser[String] = token("?")
    lazy val AMP2: Parser[String] = token("&&")
    lazy val BAR2: Parser[String] = token("||")
    lazy val BAR: Parser[String] = token("|")
    lazy val KEYWORDS: Set[String] = Set(
      "<", ">", "<=", ">=", "+", "-", "*", "/", "{", "}", "[", "]", ":", "?",
      "if", "else", "while", "foreach", "import", "cleanup", "true", "false", "in", ",", ".",
      "class", "def", "val", "mutable", "record", "=", "==", "=>", "new", "&", "|", "&&", "||"
    )

    lazy val typeAnnotation: Parser[Type] = COLON >> typeDescription

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
      case "String" => true
      case _ => false
    }

    lazy val typeVariable: Parser[TVariable] = qident ^^ { id => TVariable(id) }

    lazy val typeDescription: Parser[Type] = rule(
      qident ^^ { id => TVariable(id) }
        | ((CL(LPAREN) >> typeDescription.repeat0By(CL(COMMA)) << CL(RPAREN)) << CL(ARROW1)) ~ typeDescription ^^ { case args ~ returnType => TFunction(args, returnType) }
        | (SHARP >> sident).filter{s => !isBuiltinType(s)} ~ (CL(LT) >> typeDescription.repeat0By(CL(COMMA)) << CL(GT)).? ^^ {
        case name ~ Some(args) => TRecordReference(name, args)
        case name ~ None => TRecordReference(name, Nil)
      } | sident.filter {s => !isBuiltinType(s) } ~ (CL(LT) >> typeDescription.repeat0By(CL(COMMA)) << CL(GT)).? ^^ {
        case name ~ Some(args) => TConstructor(name, args)
        case name ~ None => TConstructor(name, Nil)
      } | token("Byte") ^^ { _ => TByte }
        | token("Short") ^^ { _ => TShort }
        | token("Int") ^^ { _ => TInt }
        | token("Long") ^^ { _ => TLong }
        | token("Float") ^^ { _ => TFloat }
        | token("Double") ^^ { _ => TDouble }
        | token("Boolean") ^^ { _ => TBoolean }
        | token("Unit") ^^ { _ => TUnit }
        | token("String") ^^ {_ => TString }
        | token("*") ^^ { _ => TDynamic }
    )

    override def root: Parser[Program] = rule(program)

    lazy val program: Parser[Program] = rule{
      (SPACING >> %%) ~ `import`.repeat0By(TERMINATOR) ~ record.repeat0By(TERMINATOR) ~ (lines << (TERMINATOR).?) << EOF ^^ {
        case location ~ imports ~ records ~ block => Program(location, imports, records, block)
      }
    }

    lazy val `import`: Parser[Import] = rule{
      (%% << CL(IMPORT)) ~ fqcn.commit ^^ { case location ~ fqcn =>
        val fragments = fqcn.split(".")
        Import(location, fragments(fragments.length - 1), fqcn)
      }
    }

    lazy val record: Parser[RecordDeclaration] = rule {
      for {
        location <- %%
        _ <- RECORD
        name <- commit(sident)
        ts <- commit((CL(LT) >> typeVariable.repeat1By(CL(COMMA)) << CL(GT)).?)
        _ <- commit(CL(LBRACE))
        members <- commit(((sident ~ CL(typeAnnotation << SEMICOLON.?)) ^^ { case n ~ t => (n, t) }).*)
        methods <- commit(methodDefinition.repeat0By(TERMINATOR))
        _ <- commit(RBRACE)
      } yield RecordDeclaration(location, name, ts.getOrElse(Nil), members, methods)
    }

    //lines ::= line {TERMINATOR expr} [TERMINATOR]
    lazy val lines: Parser[Block] = rule {
      SPACING >> (%% ~ line.repeat0By(TERMINATOR) << TERMINATOR.? ^^ { case location ~ expressions =>
        Block(location, expressions)
      })
    }

    lazy val variantDeclaration: Parser[VariantDeclaration] = rule {
      for {
        location <- %%
        _ <- CL(VARIANT)
        name <- sident
        ts <- (LT >> typeAnnotation.repeat1By(CL(COMMA)) << GT << EQ).?
        cs <- dataConstructor.*
      } yield VariantDeclaration(location, name, ts.getOrElse(Nil), cs)
    }

    lazy val dataConstructor: Parser[DataConstructor] = rule {
      (BAR >> sident) ~ (LPAREN >> ((sident << CL(COLON)) ~ typeAnnotation ^^ { case i ~ t => FormalParameter(i, t) }).repeat1By(CL(COMMA)) << RPAREN).? ^^ {
        case name ~ Some(params) => DataConstructor(name, params)
        case name ~ None => DataConstructor(name, Nil)
      }
    }

    //line ::= expression | valDeclaration | functionDefinition
    lazy val line: Parser[AST] = rule(variantDeclaration | expression | valDeclaration | functionDefinition)

    //expression ::= assignment | infix | ifExpression | whileEpression | foreachExpression
    lazy val expression: Parser[AST] = rule(assignment | infix | ifExpression | whileExpression | foreachExpression)

    //ifExpression ::= "if" "(" expression ")" expression "else" expression
    lazy val ifExpression: Parser[AST] = rule{
      (%% << CL(IF) << CL(LPAREN)) ~ commit(expression ~ CL(RPAREN) ~ expression ~ CL(ELSE) ~ expression) ^^ {
        case location ~ (condition ~ _ ~ positive ~ _ ~ negative) => IfExpression(location, condition, positive, negative)
      }
    }

    //whileExpression ::= "while" "(" expression ")" expression
    lazy val whileExpression: Parser[AST] = rule{
      (%% << CL(WHILE) << CL(LPAREN)) ~ commit(expression ~ (CL(RPAREN) >> expression)) ^^ {
        case location ~ (condition ~ body) => WhileExpression(location, condition, body)
      }
    }

    //foreachExpression ::= "foreach" "(" ident "in" expression ")" expression
    lazy val foreachExpression: Parser[AST] = rule {
      (%% << CL(FOREACH) << CL(LPAREN)) ~ commit((CL(ident) << CL(IN)) ~ (expression << CL(RPAREN)) ~ expression) ^^ {
        case location ~ (variable ~ collection ~ body) => ForeachExpression(location, variable.name, collection, body)
      }
    }

    lazy val infix: Parser[AST] = rule {
      chainl(logical)(
        (%% ~ CL(operator)) ^^ { case location ~ op => (lhs: AST, rhs: AST) => FunctionCall(location, FunctionCall(location, Id(location, op), List(lhs)), List(rhs)) }
          | (%% ~ CL(selector)) ^^ { case location ~ sl => (lhs: AST, rhs: AST) => FunctionCall(location, FunctionCall(location, sl, List(lhs)), List(rhs)) }
      )
    }

    lazy val logical: Parser[AST] = rule {
      chainl(conditional)(
        (%% << CL(AMP2)) ^^ { location => (lhs: AST, rhs: AST) => BinaryExpression(location, Operator.AND2, lhs, rhs) }
          | (%% << CL(BAR2)) ^^ { location => (lhs: AST, rhs: AST) => BinaryExpression(location, Operator.BAR2, lhs, rhs) }
      )
    }

    //conditional ::= add {"==" add | "<=" add | "=>" add | "<" add | ">" add}
    lazy val conditional: Parser[AST] = rule{
      chainl(add)(
        (%% << CL(EQEQ)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.EQUAL, left, right) } |
          (%% << CL(LTE)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.LESS_OR_EQUAL, left, right) } |
          (%% << CL(GTE)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.GREATER_EQUAL, left, right) } |
          (%% << CL(LT)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.LESS_THAN, left, right) } |
          (%% << CL(GT)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.GREATER_THAN, left, right) }
      )
    }

    //add ::= term {"+" term | "-" term}
    lazy val add: Parser[AST] = rule{
      chainl(term)(
        (%% << CL(PLUS)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.ADD, left, right) } |
          (%% << CL(MINUS)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.SUBTRACT, left, right) }
      )
    }

    //term ::= factor {"*" factor | "/" factor}
    lazy val term: Parser[AST] = rule{
      chainl(unary)(
        (%% << CL(ASTER)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.MULTIPLY, left, right) } |
          (%% << CL(SLASH)) ^^ { location => (left: AST, right: AST) => BinaryExpression(location, Operator.DIVIDE, left, right) }
      )
    }

    lazy val unary: Parser[AST] = rule(
      %% ~ CL(MINUS) ~ unary ^^ { case location ~ _ ~ operand => MinusOp(location, operand) }
    | %% ~ CL(PLUS) ~ unary ^^ { case location ~ _ ~ operand => PlusOp(location, operand) }
    | invocation
    )

    lazy val invocation: Parser[AST] = rule(%% ~ recordSelect ~ ((CL(ARROW2) >> ident) ~ (CL(LPAREN) >> expression.repeat0By(CL(COMMA)) << RPAREN).?).* ^^ {
      case location ~ self ~ Nil =>
        self
      case location ~ self ~ npList =>
        npList.foldLeft(self) { case (self, name ~ params) => MethodCall(location, self, name.name, params.getOrElse(Nil)) }
    })

    lazy val recordSelect: Parser[AST] = rule(%% ~ application ~ ((CL(DOT) >> (%% ~ ident ~ (CL(LPAREN) >> expression.repeat0By((COMMA)) << RPAREN).?))).* ^^ {
      case location ~ self ~ names =>
        val ns = names.map {
          case l ~ n ~ Some(params) => (l, n.name, Some(params))
          case l ~ n ~ None => (l, n.name, None)
        }
        ns.foldLeft(self) { case (e, (l, n, optParams)) =>
          optParams match {
            case Some(params) => RecordCall(l, e, n, params)
            case None => RecordSelect(l, e, n)
          }
        }
    })

    lazy val application: Parser[AST] = rule{
      %% ~ pipelinable ~ (
        blockFunctionParameter
      | parenthesizedParameter
      ).* ^^ {
        case location ~ f ~ params =>
          params.foldLeft(f:AST) {(f, params) =>
            FunctionCall(location, f, params)
          }
      }
    }

    lazy val pipelinable: Parser[AST] = rule(%% ~ castable ~ (CL(BAR) >> ident).? ^^ {
      case location ~ self ~ None =>
        self
      case location ~ self ~ Some(name) =>
        FunctionCall(location, name, List(self))
    })

    lazy val castable: Parser[AST] = rule(primary ~ ((%% << CL(COLONGT)) ~ CL(castType)).? ^^ {
      case target ~ Some((location ~ castType)) => Casting(location, target, castType)
      case target ~ None => target
    })

    //primary ::= selector | booleanLiteral | ident | floatLiteral | integerLiteral | mapLiteral | stringLiteral | listLiteral | setLiteral | newRecord | newObject | functionLiteral | "(" expression ")" | "{" lines "}"
    lazy val primary: Parser[AST] = rule{(
      selector
    | booleanLiteral
    | ident
    | floatLiteral
    | integerLiteral
    | newRecord
    | newObject
    | functionLiteral
    | predict(
        '%' -> mapLiteral,
        '#' -> setLiteral,
        '"' -> stringLiteral,
        '[' -> listLiteral,
        '(' -> (CL(LPAREN) >> expression << RPAREN),
        '{' -> (CL(LBRACE) >> lines << RBRACE)
      )
    )}

    //integerLiteral ::= ["1"-"9"] {"0"-"9"}
    lazy val integerLiteral: Parser[AST] = (%% ~ """[1-9][0-9]*|0""".r ~ ("BY" ^^ { _ => ByteSuffix } | "L" ^^ { _ => LongSuffix } | "S" ^^ { _ => ShortSuffix }).? ^^ {
      case location ~ value ~ None => IntNode(location, value.toLong.toInt)
      case location ~ value ~ Some(LongSuffix) => LongNode(location, value.toLong)
      case location ~ value ~ Some(ShortSuffix) => ShortNode(location, value.toShort)
      case location ~ value ~ Some(ByteSuffix) => ByteNode(location, value.toByte)
    }) << SPACING_WITHOUT_LF

    lazy val floatLiteral: Parser[AST] = ((%% ~ "([1-9][0-9]*|0)\\.[0-9]*".r ~ ("F" ^^ { _ => FloatSuffix }).?) ^^ {
      case location ~ value ~ None => DoubleNode(location, value.toDouble)
      case location ~ value ~ Some(FloatSuffix) => FloatNode(location, value.toFloat)
    }) << SPACING_WITHOUT_LF

    lazy val booleanLiteral: Parser[AST] = %% ~ (TRUE ^^ { _ => true } | FALSE ^^ { _ => false }) ^^ {
      case location ~ true => BooleanNode(location, true)
      case location ~ false => BooleanNode(location, false)
    }

    //stringLiteral ::= "\"" ((?!")(\[rntfb"'\\]|[^\\]))* "\""
    lazy val stringLiteral: Parser[AST] =
      ("\"" >>
        (%% ~ """((?!("|#\{))(\\[rntfb"'\\]|[^\\]))+""".r ^^ { case location ~ in =>
          StringNode(location, unescape(in))
        } | "#{" >> expression << "}"
          ).*
        << "\"" ^^ { values =>
        values.foldLeft(StringNode(NoLocation, ""): AST) { (node, content) => BinaryExpression(content.location, Operator.ADD, node, content) }
      }) << SPACING_WITHOUT_LF

    lazy val listLiteral: Parser[AST] = rule(%% ~ (CL(LBRACKET) >> commit((CL(expression).repeat0By(SEPARATOR) << SEPARATOR.?) << RBRACKET)) ^^ {
      case location ~ contents => ListLiteral(location, contents)
    })

    lazy val setLiteral: Parser[AST] = rule(%% ~ (CL(SET_OPEN) >> commit((CL(expression).repeat0By(SEPARATOR) << SEPARATOR.?) << RPAREN)) ^^ {
      case location ~ contents => SetLiteral(location, contents)
    })

    lazy val mapLiteral: Parser[AST] = rule(%% ~ (CL(MAP_OPEN) >> commit((CL(expression ~ COLON ~ expression).repeat0By(SEPARATOR) << SEPARATOR.?) << RBRACKET)) ^^ {
      case location ~ contents => MapLiteral(location, contents.map { case k ~ colon ~ v => (k, v) })
    })

    lazy val fqcn: Parser[String] = (ident ~ (CL(DOT) ~ ident).*) ^^ {
      case id ~ ids => ids.foldLeft(id.name) { case (a, d ~ e) => a + d + e.name }
    }

    lazy val component: Parser[String] = """[A-Za-z_][a-zA-Z0-9_]*""".r

    lazy val ident: Parser[Id] = (%% ~ component.filter{n =>
      !KEYWORDS(n)
    } ^^ { case location ~ name => Id(location, name) }) << SPACING_WITHOUT_LF

    def selector: Parser[Selector] = rule(((%% ~ component ~ "#" ~ component).filter{ case (_ ~ m ~ _ ~ n) =>
      (!KEYWORDS(m)) && (!KEYWORDS(n))
    } ^^ { case location ~ m ~ _ ~ n => Selector(location, m, n) }) << SPACING_WITHOUT_LF)

    lazy val qident: Parser[String] = (regularExpression("""'[A-Za-z_][a-zA-Z0-9_]*""".r).filter{n =>
      !KEYWORDS(n)
    }) << SPACING_WITHOUT_LF

    lazy val sident: Parser[String] = (regularExpression("""[A-Za-z_][a-zA-Z0-9_]*""".r).filter{n =>
      !KEYWORDS(n)
    }) << SPACING_WITHOUT_LF

    lazy val operator: Parser[String] = (regularExpression("""#[A-Za-z_][a-zA-Z0-9_]*""".r).filter{n =>
      !KEYWORDS(n.substring(1))
    }.map{n => n.substring(1)}) << SPACING_WITHOUT_LF

    lazy val assignment: Parser[Assignment] = rule(ident ~ CL(PLUSEQ | MINUSEQ | ASTEREQ | SLASHEQ | EQ) ~ commit(expression) ^^ {
      case v ~ "=" ~ value => SimpleAssignment(v.location, v.name, value)
      case v ~ "+=" ~ value => PlusAssignment(v.location, v.name, value)
      case v ~ "-=" ~ value => MinusAssignment(v.location, v.name, value)
      case v ~ "*=" ~ value => MultiplicationAssignment(v.location, v.name, value)
      case v ~ "/=" ~ value => DivisionAssignment(v.location, v.name, value)
      case _ ~ op ~ _ => sys.error(s"unknown assignment operator ${op}")
    })

    // valDeclaration ::= "val" ident "=" expression
    lazy val valDeclaration: Parser[ValDeclaration] = rule((%% ~ CL(MUTABLE ^^ { _ => false } | VAL ^^ { _ => true })) ~ commit(ident ~ (typeAnnotation.? << CL(EQ)) ~ expression) ^^ {
      case location ~ immutable ~ (valName ~ optionalType ~ value) => ValDeclaration(location, valName.name, optionalType, value, immutable)
    })

    // parenthesizedParameter ::= "(" [param {"," param}] ")"
    lazy val parenthesizedParameter: Parser[List[AST]] = rule{
      CL(LPAREN) >> CL(expression).repeat0By(CL(COMMA)) << (SPACING << RPAREN) ^^ {
        case xs => xs
      }
    }

    // blockFunctionParameter ::= "{" [param {"," param}] "=>" expression "}"
    lazy val blockFunctionParameter: Parser[List[AST]] = rule{
      (%% << CL(LBRACE)) ~ (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) ~ (typeAnnotation.? << CL(ARROW1)) ~ (expression << RBRACE) ^^ {
        case location ~ params ~ optionalType ~ body =>
          List(
            Lambda(
              location,
              params.map {
                case name ~ Some(type_) => FormalParameterOptional(name.name, Some(type_))
                case name ~ None => FormalParameterOptional(name.name, None)
              },
              optionalType,
              body
            )
          )
      }
    }

    // functionLiteral ::= "(" [param {"," param}] ")" "=>" expression
    lazy val functionLiteral: Parser[AST] = rule(%% ~ (CL(LPAREN) >> (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) << CL(RPAREN)).? ~ (typeAnnotation.? << CL(ARROW1)) ~ expression ^^ {
      case location ~ Some(params) ~ optionalType ~ body =>
        Lambda(
          location,
          params.map {
            case name ~ Some(type_) => FormalParameterOptional(name.name, Some(type_))
            case name ~ None => FormalParameterOptional(name.name, None)
          },
          optionalType,
          body
        )
      case location ~ None ~ optionalType ~ body => Lambda(location, List(), optionalType, body)
    })

    // newObject ::= "new" fqcn "(" [param {"," param} ")"
    lazy val newObject: Parser[AST] = rule((%% << CL(NEW)) ~ commit(fqcn ~ (CL(LPAREN) >> expression.repeat0By(CL(COMMA)) << RPAREN).?) ^^ {
      case location ~ (className ~ Some(params)) => ObjectNew(location, className, params)
      case location ~ (className ~ None) => ObjectNew(location, className, List())
    })

    // newRecord ::= "#" sident "(" [param {"," param} ")"
    lazy val newRecord: Parser[AST] = rule((%% << CL(SHARP)) ~ commit(sident ~ (CL(LPAREN) >> expression.repeat0By(CL(COMMA)) << RPAREN).?) ^^ {
      case location ~ (recordName ~ Some(params)) => RecordNew(location, recordName, params)
      case location ~ (recordName ~ None) => RecordNew(location, recordName, List())
    })

    // methodDefinition ::= "def" ident  ["(" [param {"," param}] ")"] "=" expression
    lazy val methodDefinition: Parser[MethodDefinition] = rule {
      (%% << CL(DEF)) ~ commit(ident ~ (CL(LPAREN) >> (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) << CL(RPAREN)).? ~ (typeAnnotation.? << CL(EQ)) ~ expression ~ (CL(CLEANUP) >> expression).?) ^^ {
        case location ~ (functionName ~ params ~ optionalType ~ body ~ cleanup) =>
          val ps = params match {
            case Some(xs) =>
              xs.map {
                case name ~ Some(annotation) => FormalParameterOptional(name.name, Some(annotation))
                case name ~ None => FormalParameterOptional(name.name, None)
              }
            case None => Nil
          }
          MethodDefinition(
            location,
            functionName.name,
            Lambda(body.location, ps, optionalType, body),
            cleanup
          )
      }
    }

    // functionDefinition ::= "def" ident  ["(" [param {"," param}] ")"] "=" expression
    lazy val functionDefinition: Parser[FunctionDefinition] = rule {
      (%% << CL(DEF)) ~ commit(ident ~ (CL(LPAREN) >> (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) << CL(RPAREN)).? ~ (typeAnnotation.? << CL(EQ)) ~ expression ~ (CL(CLEANUP) >> expression).?) ^^ {
        case location ~ (functionName ~ params ~ optionalType ~ body ~ cleanup) =>
          val ps = params match {
            case Some(xs) =>
              xs.map {
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
    }
  }

  import KlassicParsers._

  def parseExpression(input: String): AST = {
    parse(input) match {
      case Result.Success(program) => program.block
      case Result.Failure(location, message) => throw new InterpreterException(s"${location}:${message}")
    }
  }

  def parseAll(input: String): Program = {
    parse(input) match {
      case Result.Success(program) => program
      case Result.Failure(location, message) => throw new InterpreterException(s"${location}:${message}")
    }
  }

  override final val name: String = "Parser"

  override final def process(input: String): Program = parseAll(input)
}
