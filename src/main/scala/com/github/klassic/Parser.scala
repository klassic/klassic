package com.github.klassic


import scala.util.matching.Regex
import com.github.klassic.Ast._
import com.github.klassic.Type._
import com.github.klassic.macro_peg.Parser.Fragment
import com.github.kmizu.scomb
import com.github.kmizu.scomb.{Result, SCombinator}

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class Parser extends Processor[String, Program, InteractiveSession] {
  object Core extends SCombinator with macro_peg.Parser.Fragment {
    object Klassic {
      def publicLocations: mutable.Map[Int, scomb.Location] = locations

      implicit def stringToParser(literal: String): Parser[String] = $(literal)

      implicit def regexToParser(literal: Regex): Parser[String] = regularExpression(literal)

      def %% : Parser[SourceLocation] = % ^^ { l =>
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

      lazy val COMMENT: Parser[Any] = rule {
        BLOCK_COMMENT | LINE_COMMENT
      }

      def CL[T](parser: Parser[T]): Parser[T] = parser << SPACING

      private[this] val tokenNames = mutable.Set.empty[String]

      /**
        * Define a *keywork token*, which is regarded as a keyword
        * Note that this method must not called lazily since this method
        * has side effects.
        * @param name token name. it must not have spaces
        */
      def kwToken(name: String): Parser[String] = {
        tokenNames += name
        name << SPACING_WITHOUT_LF
      }

      /**
        * Define a *special token*, which is *not* regarded as a keyword
        * @param parser
        * @return
        */
      def spToken(parser: String): Parser[String] = parser << SPACING_WITHOUT_LF

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

      //begin token definition
      val LT: Parser[String]               = kwToken("<")
      val GT: Parser[String]               = kwToken(">")
      val LTE: Parser[String]              = kwToken("<=")
      val GTE: Parser[String]              = kwToken(">=")
      val MAP_OPEN: Parser[String]         = kwToken("%[")
      val SET_OPEN: Parser[String]         = kwToken("%(")
      val UNDERSCORE: Parser[String]       = kwToken("_")
      val PLUS: Parser[String]             = kwToken("+")
      val MINUS: Parser[String]            = kwToken("-")
      val ASTER: Parser[String]            = kwToken("*")
      val SLASH: Parser[String]            = kwToken("/")
      val LPAREN: Parser[String]           = kwToken("(")
      val RPAREN: Parser[String]           = kwToken(")")
      val LBRACE: Parser[String]           = kwToken("{")
      val RBRACE: Parser[String]           = kwToken("}")
      val LBRACKET: Parser[String]         = kwToken("[")
      val RBRACKET: Parser[String]         = kwToken("]")
      val SHARP: Parser[String]            = kwToken("#")
      val IF: Parser[String]               = kwToken("if")
      val ELSE: Parser[String]             = kwToken("else")
      val THEN: Parser[String]             = kwToken("then")
      val WHILE: Parser[String]            = kwToken("while")
      val FOREACH: Parser[String]          = kwToken("foreach")
      val IMPORT: Parser[String]           = kwToken("import")
      val ENUM: Parser[String]             = kwToken("enum")
      val TRUE: Parser[String]             = kwToken("true")
      val FALSE: Parser[String]            = kwToken("false")
      val IN: Parser[String]               = kwToken("in")
      val COMMA: Parser[String]            = kwToken(",")
      val DOT: Parser[String]              = kwToken(".")
      val CLASS: Parser[String]            = kwToken("class")
      val RECORD: Parser[String]           = kwToken("record")
      val DEF: Parser[String]              = kwToken("def")
      val MUTABLE: Parser[String]          = kwToken("mutable")
      val CLEANUP: Parser[String]          = kwToken("cleanup")
      val VAL: Parser[String]              = kwToken("val")
      val EQ: Parser[String]               = kwToken("=")
      val RULE: Parser[String]             = kwToken("rule")
      val BEGIN_MSTR: Parser[String]       = kwToken("<<<")
      val END_MSTR: Parser[String]         = kwToken(">>>")
      val PLUSEQ: Parser[String]           = kwToken("+=")
      val MINUSEQ: Parser[String]          = kwToken("-=")
      val ASTEREQ: Parser[String]          = kwToken("*=")
      val SLASHEQ: Parser[String]          = kwToken("/=")
      val COLONGT: Parser[String]          = kwToken(":>")
      val EQEQ: Parser[String]             = kwToken("==")
      val ARROW1: Parser[String]           = kwToken("=>")
      val ARROW2: Parser[String]           = kwToken("->")
      val COLON: Parser[String]            = kwToken(":")
      val NEW: Parser[String]              = kwToken("new")
      val QUES: Parser[String]             = kwToken("?")
      val AMP2: Parser[String]             = kwToken("&&")
      val BAR2: Parser[String]             = kwToken("||")
      val BAR: Parser[String]              = kwToken("|")
      val AMP: Parser[String]              = kwToken("&")
      val HAT: Parser[String]              = kwToken("^")
      // end token definition

      lazy val KEYWORDS: Set[String] = tokenNames.toSet

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
          | (SHARP >> sident).filter { s => !isBuiltinType(s) } ~ (CL(LT) >> typeDescription.repeat0By(CL(COMMA)) << CL(GT)).? ^^ {
          case name ~ Some(args) => TRecordReference(name, args)
          case name ~ None => TRecordReference(name, Nil)
        } | sident.filter { s => !isBuiltinType(s) } ~ (CL(LT) >> typeDescription.repeat0By(CL(COMMA)) << CL(GT)).? ^^ {
          case name ~ Some(args) => TConstructor(name, args)
          case name ~ None => TConstructor(name, Nil)
        } | kwToken("Byte") ^^ { _ => TByte }
          | kwToken("Short") ^^ { _ => TShort }
          | kwToken("Int") ^^ { _ => TInt }
          | kwToken("Long") ^^ { _ => TLong }
          | kwToken("Float") ^^ { _ => TFloat }
          | kwToken("Double") ^^ { _ => TDouble }
          | kwToken("Boolean") ^^ { _ => TBoolean }
          | kwToken("Unit") ^^ { _ => TUnit }
          | kwToken("String") ^^ { _ => TString }
          | kwToken("*") ^^ { _ => TDynamic }
      )

      def root: Parser[Program] = rule(program)

      def grammar: Parser[macro_peg.Ast.Grammar] = (CL(RULE) >> CL(LBRACE)) >> MacroPeg.EMBEDDED_GRAMMAR<< CL(RBRACE)

      lazy val program: Parser[Program] = rule {
        (SPACING >> %%) ~ grammar.? ~ `import`.repeat0By(TERMINATOR) ~ record.repeat0By(TERMINATOR) ~ (lines << (TERMINATOR).?) << EOF ^^ {
          case location ~ grammar ~ imports ~ records ~ block => Program(location, grammar, imports, records, block)
        }
      }

      lazy val `import`: Parser[Import] = rule {
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

      lazy val variantDeclaration: Parser[EnumDeclaration] = rule {
        for {
          location <- %%
          _ <- CL(ENUM)
          name <- sident
          ts <- (LT >> typeAnnotation.repeat1By(CL(COMMA)) << GT << EQ).?
          cs <- dataConstructor.*
        } yield EnumDeclaration(location, name, ts.getOrElse(Nil), cs)
      }

      lazy val dataConstructor: Parser[DataConstructor] = rule {
        (BAR >> sident) ~ (LPAREN >> ((sident << CL(COLON)) ~ typeAnnotation ^^ { case i ~ t => FormalParameter(i, t) }).repeat1By(CL(COMMA)) << RPAREN).? ^^ {
          case name ~ Some(params) => DataConstructor(name, params)
          case name ~ None => DataConstructor(name, Nil)
        }
      }

      //line ::= expression | valDeclaration | functionDefinition
      lazy val line: Parser[Ast.Node] = rule(variantDeclaration | expression | valDeclaration | functionDefinition)

      //expression ::= assignment | ternary | ifExpression | whileEpression | foreachExpression
      lazy val expression: Parser[Ast.Node] = rule(assignment | ternary | ifExpression | whileExpression | foreachExpression)

      //ifExpression ::= "if" "(" expression ")" expression "else" expression
      lazy val ifExpression: Parser[Ast.Node] = rule {
        (%% << CL(IF) << CL(LPAREN)) ~ commit(expression ~ CL(RPAREN) ~ expression ~ CL(ELSE) ~ expression) ^^ {
          case location ~ (condition ~ _ ~ positive ~ _ ~ negative) => IfExpression(location, condition, positive, negative)
        }
      }

      //whileExpression ::= "while" "(" expression ")" expression
      lazy val whileExpression: Parser[Ast.Node] = rule {
        (%% << CL(WHILE) << CL(LPAREN)) ~ commit(expression ~ (CL(RPAREN) >> expression)) ^^ {
          case location ~ (condition ~ body) => WhileExpression(location, condition, body)
        }
      }

      //foreachExpression ::= "foreach" "(" ident "in" expression ")" expression
      lazy val foreachExpression: Parser[Ast.Node] = rule {
        (%% << CL(FOREACH) << CL(LPAREN)) ~ commit((CL(ident) << CL(IN)) ~ (expression << CL(RPAREN)) ~ expression) ^^ {
          case location ~ (variable ~ collection ~ body) => ForeachExpression(location, variable.name, collection, body)
        }
      }

      lazy val ternary: Parser[Ast.Node] = rule {
        for(
          location <- %%;
          condition <- infix;
          opt <- ((CL(THEN) ~> infix) ~ (CL(ELSE) ~> infix)).?
        ) yield opt match {
          case Some(th ~ el) => TernaryExpression(location, condition, th, el)
          case None => condition
        }
      }

      lazy val infix: Parser[Ast.Node] = rule {
        chainl(logicalOr)(
          (%% ~ CL(operator)) ^^ { case location ~ op => (lhs: Ast.Node, rhs: Ast.Node) => FunctionCall(location, FunctionCall(location, Id(location, op), List(lhs)), List(rhs)) }
            | (%% ~ CL(selector)) ^^ { case location ~ sl => (lhs: Ast.Node, rhs: Ast.Node) => FunctionCall(location, FunctionCall(location, sl, List(lhs)), List(rhs)) }
        )
      }

      lazy val logicalOr: Parser[Ast.Node] = rule {
        chainl(logicalAnd)(
          (%% << CL(BAR2)) ^^ { location => (lhs: Ast.Node, rhs: Ast.Node) => BinaryExpression(location, Operator.BAR2, lhs, rhs) }
        )
      }

      lazy val logicalAnd: Parser[Ast.Node] = rule {
        chainl(bitOr)(
          (%% << CL(AMP2)) ^^ { location => (lhs: Ast.Node, rhs: Ast.Node) => BinaryExpression(location, Operator.AND2, lhs, rhs) }
        )
      }

      lazy val bitOr: Parser[Ast.Node] = rule {
        chainl(bitXor)(
          (%% << CL(BAR)) ^^ { location => (lhs: Ast.Node, rhs: Ast.Node) => BinaryExpression(location, Operator.OR, lhs, rhs) }
        )
      }

      lazy val bitXor: Parser[Ast.Node] = rule {
        chainl(bitAnd)(
          (%% << CL(HAT)) ^^ { location => (lhs: Ast.Node, rhs: Ast.Node) => BinaryExpression(location, Operator.XOR, lhs, rhs) }
        )
      }

      lazy val bitAnd: Parser[Ast.Node] = rule {
        chainl(conditional)(
          (%% << CL(AMP)) ^^ { location => (lhs: Ast.Node, rhs: Ast.Node) => BinaryExpression(location, Operator.AND, lhs, rhs) }
        )
      }

      //conditional ::= add {"==" add | "<=" add | "=>" add | "<" add | ">" add}
      lazy val conditional: Parser[Ast.Node] = rule {
        chainl(add)(
          (%% << CL(EQEQ)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.EQUAL, left, right) } |
            (%% << CL(LTE)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.LESS_OR_EQUAL, left, right) } |
            (%% << CL(GTE)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.GREATER_EQUAL, left, right) } |
            (%% << CL(LT)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.LESS_THAN, left, right) } |
            (%% << CL(GT)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.GREATER_THAN, left, right) }
        )
      }

      //add ::= term {"+" term | "-" term}
      lazy val add: Parser[Ast.Node] = rule {
        chainl(term)(
          (%% << CL(PLUS)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.ADD, left, right) } |
            (%% << CL(MINUS)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.SUBTRACT, left, right) }
        )
      }

      //term ::= factor {"*" factor | "/" factor}
      lazy val term: Parser[Ast.Node] = rule {
        chainl(unary)(
          (%% << CL(ASTER)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.MULTIPLY, left, right) } |
            (%% << CL(SLASH)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.DIVIDE, left, right) }
        )
      }

      lazy val unary: Parser[Ast.Node] = rule(
        %% ~ CL(MINUS) ~ unary ^^ { case location ~ _ ~ operand => MinusOp(location, operand) }
          | %% ~ CL(PLUS) ~ unary ^^ { case location ~ _ ~ operand => PlusOp(location, operand) }
          | invocation
      )

      lazy val invocation: Parser[Ast.Node] = rule(%% ~ recordSelect ~ ((CL(ARROW2) >> ident) ~ (CL(LPAREN) >> expression.repeat0By(CL(COMMA)) << RPAREN).?).* ^^ {
        case location ~ self ~ Nil =>
          self
        case location ~ self ~ npList =>
          npList.foldLeft(self) { case (self, name ~ params) => MethodCall(location, self, name.name, params.getOrElse(Nil)) }
      })

      lazy val recordSelect: Parser[Ast.Node] = rule(%% ~ application ~ ((CL(DOT) >> (%% ~ ident ~ (CL(LPAREN) >> expression.repeat0By((COMMA)) << RPAREN).?))).* ^^ {
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

      lazy val application: Parser[Ast.Node] = rule {
        %% ~ pipelinable ~ (
          blockFunctionParameter
            | parenthesizedParameter
          ).* ^^ {
          case location ~ f ~ params =>
            params.foldLeft(f: Ast.Node) { (f, params) =>
              FunctionCall(location, f, params)
            }
        }
      }

      lazy val pipelinable: Parser[Ast.Node] = rule(%% ~ castable ~ (CL(BAR) >> ident).? ^^ {
        case location ~ self ~ None =>
          self
        case location ~ self ~ Some(name) =>
          FunctionCall(location, name, List(self))
      })

      lazy val castable: Parser[Ast.Node] = rule(primary ~ ((%% << CL(COLONGT)) ~ CL(castType)).? ^^ {
        case target ~ Some((location ~ castType)) => Casting(location, target, castType)
        case target ~ None => target
      })

      //primary ::= selector | booleanLiteral | ident | floatLiteral | integerLiteral | mapLiteral | stringLiteral | listLiteral | setLiteral | newRecord | newObject | functionLiteral | "(" expression ")" | "{" lines "}"
      lazy val primary: Parser[Ast.Node] = rule {
        (
          selector
            | booleanLiteral
            | placeholder
            | ident
            | floatLiteral
            | integerLiteral
            | newRecord
            | newObject
            | functionLiteral
            | predict(
            '%' -> (mapLiteral | setLiteral),
            '"' -> stringLiteral,
            '[' -> listLiteral,
            '(' -> (CL(LPAREN) >> expression << RPAREN),
            '{' -> (CL(LBRACE) >> lines << RBRACE)
          )
          )
      }

      //integerLiteral ::= ["1"-"9"] {"0"-"9"}
      lazy val integerLiteral: Parser[Ast.Node] = (%% ~ """[1-9][0-9]*|0""".r ~ ("BY" ^^ { _ => ByteSuffix } | "L" ^^ { _ => LongSuffix } | "S" ^^ { _ => ShortSuffix }).? ^^ {
        case location ~ value ~ None => IntNode(location, value.toLong.toInt)
        case location ~ value ~ Some(LongSuffix) => LongNode(location, value.toLong)
        case location ~ value ~ Some(ShortSuffix) => ShortNode(location, value.toShort)
        case location ~ value ~ Some(ByteSuffix) => ByteNode(location, value.toByte)
      }) << SPACING_WITHOUT_LF

      lazy val floatLiteral: Parser[Ast.Node] = ((%% ~ "([1-9][0-9]*|0)\\.[0-9]*".r ~ ("F" ^^ { _ => FloatSuffix }).?) ^^ {
        case location ~ value ~ None => DoubleNode(location, value.toDouble)
        case location ~ value ~ Some(FloatSuffix) => FloatNode(location, value.toFloat)
      }) << SPACING_WITHOUT_LF

      lazy val booleanLiteral: Parser[Ast.Node] = %% ~ (TRUE ^^ { _ => true } | FALSE ^^ { _ => false }) ^^ {
        case location ~ true => BooleanNode(location, true)
        case location ~ false => BooleanNode(location, false)
      }

      //stringLiteral ::= "\"" ((?!")(\[rntfb"'\\]|[^\\]))* "\""
      lazy val stringLiteral: Parser[Ast.Node] =
        ("\"" >>
          (%% ~ """((?!("|#\{))(\\[rntfb"'\\]|[^\\]))+""".r ^^ { case location ~ in =>
            StringNode(location, unescape(in))
          } | "#{" >> expression << "}"
            ).*
          << "\"" ^^ { values =>
          values.foldLeft(StringNode(NoLocation, ""): Ast.Node) { (node, content) => BinaryExpression(content.location, Operator.ADD, node, content) }
        }) << SPACING_WITHOUT_LF

      lazy val listLiteral: Parser[Ast.Node] = rule(%% ~ (CL(LBRACKET) >> commit((CL(expression).repeat0By(SEPARATOR) << SEPARATOR.?) << RBRACKET)) ^^ {
        case location ~ contents => ListLiteral(location, contents)
      })

      lazy val setLiteral: Parser[Ast.Node] = rule(%% ~ (CL(SET_OPEN) >> commit((CL(expression).repeat0By(SEPARATOR) << SEPARATOR.?) << RPAREN)) ^^ {
        case location ~ contents => SetLiteral(location, contents)
      })

      lazy val mapLiteral: Parser[Ast.Node] = rule(%% ~ (CL(MAP_OPEN) >> commit((CL(expression ~ COLON ~ expression).repeat0By(SEPARATOR) << SEPARATOR.?) << RBRACKET)) ^^ {
        case location ~ contents => MapLiteral(location, contents.map { case k ~ colon ~ v => (k, v) })
      })

      lazy val fqcn: Parser[String] = (ident ~ (CL(DOT) ~ ident).*) ^^ {
        case id ~ ids => ids.foldLeft(id.name) { case (a, d ~ e) => a + d + e.name }
      }

      lazy val component: Parser[String] = """[A-Za-z_][a-zA-Z0-9_]*""".r

      lazy val placeholder: Parser[Placeholder] = ((%% ~ UNDERSCORE) ^^ { case location ~ _ => Placeholder(location) }) << SPACING_WITHOUT_LF

      lazy val ident: Parser[Id] = (%% ~ component.filter { n =>
        !KEYWORDS(n)
      } ^^ { case location ~ name => Id(location, name) }) << SPACING_WITHOUT_LF

      def selector: Parser[Selector] = rule(((%% ~ component ~ "#" ~ component).filter { case (_ ~ m ~ _ ~ n) =>
        (!KEYWORDS(m)) && (!KEYWORDS(n))
      } ^^ { case location ~ m ~ _ ~ n => Selector(location, m, n) }) << SPACING_WITHOUT_LF)

      lazy val qident: Parser[String] = (regularExpression("""'[A-Za-z_][a-zA-Z0-9_]*""".r).filter { n =>
        !KEYWORDS(n)
      }) << SPACING_WITHOUT_LF

      lazy val sident: Parser[String] = (regularExpression("""[A-Za-z_][a-zA-Z0-9_]*""".r).filter { n =>
        !KEYWORDS(n)
      }) << SPACING_WITHOUT_LF

      lazy val operator: Parser[String] = (regularExpression("""#[A-Za-z_][a-zA-Z0-9_]*""".r).filter { n =>
        !KEYWORDS(n.substring(1))
      }.map { n => n.substring(1) }) << SPACING_WITHOUT_LF

      lazy val assignment: Parser[Assignment] = rule(ident ~ CL(PLUSEQ | MINUSEQ | ASTEREQ | SLASHEQ | EQ) ~ expression ^^ {
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
      lazy val parenthesizedParameter: Parser[List[Ast.Node]] = rule {
        CL(LPAREN) >> CL(expression).repeat0By(CL(COMMA)) << (SPACING << RPAREN) ^^ {
          case xs => xs
        }
      }

      // blockFunctionParameter ::= "{" [param {"," param}] "=>" expression "}"
      lazy val blockFunctionParameter: Parser[List[Ast.Node]] = rule {
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
      lazy val functionLiteral: Parser[Ast.Node] = rule(%% ~ (CL(LPAREN) >> (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) << CL(RPAREN)).? ~ (typeAnnotation.? << CL(ARROW1)) ~ expression ^^ {
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
      lazy val newObject: Parser[Ast.Node] = rule((%% << CL(NEW)) ~ commit(fqcn ~ (CL(LPAREN) >> expression.repeat0By(CL(COMMA)) << RPAREN).?) ^^ {
        case location ~ (className ~ Some(params)) => ObjectNew(location, className, params)
        case location ~ (className ~ None) => ObjectNew(location, className, List())
      })

      // newRecord ::= "#" sident "(" [param {"," param} ")"
      lazy val newRecord: Parser[Ast.Node] = rule((%% << CL(SHARP)) ~ commit(sident ~ (CL(LPAREN) >> expression.repeat0By(CL(COMMA)) << RPAREN).?) ^^ {
        case location ~ (recordName ~ Some(params)) => RecordNew(location, recordName, params)
        case location ~ (recordName ~ None) => RecordNew(location, recordName, List())
      })

      // methodDefinition ::= "def" ident  ["(" [param {"," param}] ")"] "=" expression
      lazy val methodDefinition: Parser[MethodDefinition] = rule {
        (%% << CL(DEF)) ~ commit(ident ~ (CL(LPAREN) >> (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) << CL(RPAREN)).? ~ (typeAnnotation.? << CL(EQ)) ~ expression ~ (CLEANUP >> expression).?) ^^ {
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
  }

  import Core._

  def parsePeg(input: String): macro_peg.Ast.Grammar = {
    parse(MacroPeg.GRAMMAR, input) match {
      case Result.Success(grammar) => grammar
      case Result.Failure(location, message) => throw new InterpreterException(s"${location}:${message}")
    }
  }

  def parseExpression(input: String): Ast.Node = {
    parse(Klassic.root, input) match {
      case Result.Success(program) => program.block
      case Result.Failure(location, message) => throw new InterpreterException(s"${location}:${message}")
    }
  }

  def parseAll(input: String): Program = {
    parse(Klassic.root, input) match {
      case Result.Success(program) => program
      case Result.Failure(location, message) => throw new InterpreterException(s"${location}:${message}")
    }
  }

  override final val name: String = "Parser"

  override final def process(input: String, session: InteractiveSession): Program = parseAll(input)
}
