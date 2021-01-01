package fuselang

import scala.util.parsing.input.{Positional, OffsetPosition}

import fastparse._
import fastparse.JavaWhitespace._

import fuselang.common._
import Syntax._

case class Parser(input: String) {

  // Common surround expressions
  def braces[_: P, T](p: => P[T]): P[T] = P("{" ~/ p ~ "}")
  def brackets[_: P, T](p: => P[T]): P[T] = P("[" ~ p ~ "]")
  def angular[_: P, T](p: => P[T]): P[T] = P("<" ~/ p ~ ">")
  def parens[_: P, T](p: => P[T]): P[T] = P("(" ~/ p ~ ")")

  def positioned[_: P, T <: Positional](p: => P[T]): P[T] = {
    P(Index ~ p).map({
      case (index, t) => t.setPos(OffsetPosition(input, index))
    })
  }

  /*def notKws[_: P] = {
    import fastparse.NoWhitespace._
    P(!(StringIn(
      "float", "double", "bool", "bit", "ubit", "fix", "ufix", "let", "for",
      "bank", "unroll", "decl", "true", "false", "as", "pipeline", "combine",
      "if", "else", "import", "decor", "def", "record", "split", "view",
      "return", "while"
    ) ~ &(" "))).opaque("non reserved keywords")
  }*/

  def kw[_: P](word: String): P[Unit] = {
    import fastparse.NoWhitespace._
    P(word ~ !CharsWhileIn("a-zA-Z0-9_"))
  }

  // Basic atoms
  def iden[_: P]: P[Id] = {
    import fastparse.NoWhitespace._
    positioned(P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").?).!.map({
      case rest => Id(rest)
    }).opaque("Expected valid identifier"))
  }

  def number[_: P]: P[Int] =
    P(CharIn("0-9").rep(1).!.map(_.toInt)).opaque("Expected positive number")

  def stringVal[_: P]: P[String] =
    P("\"" ~/ CharPred(_ != '"').rep.! ~ "\"")

  // Types
  def typAtom[_: P]: P[Type] =
    positioned(
      P(
        kw("float").!.map(_ => TFloat()) |
          kw("double").!.map(_ => TDouble()) |
          kw("bool").!.map(_ => TBool()) |
          (kw("bit") ~/ angular(number)).map(s => TSizedInt(s, false)) |
          (kw("ubit") ~/ angular(number)).map(s => TSizedInt(s, true)) |
          (kw("fix") ~/ angular(number ~ "," ~ number)).map({
            case (s1, s2) => TFixed(s1, s2, false)
          }) |
          (kw("ufix") ~/ angular(number ~ "," ~ number)).map({
            case (s1, s2) => TFixed(s1, s2, true)
          }) |
          iden.map(TAlias(_))
      )
    )

  def typIdx[_: P]: P[DimSpec] =
    P(brackets(number ~ (kw("bank") ~ number).?)).map({
      case (n, b) => (n, b.getOrElse(1))
    })

  def typ[_: P]: P[Type] =
    positioned(P(typAtom ~ (braces(number).? ~ typIdx.rep(1)).?).map({
      case (typ, Some((ports, dims))) =>
        TArray(typ, dims.toList, ports.getOrElse(1))
      case (typ, None) => typ
    }))

  // Literals
  def uInt[_: P]: P[Expr] =
    positioned(
      P(
        "0" | "-".? ~ (CharIn("1-9") ~ CharsWhileIn("0-9").?)
      ).!.map((n: String) => EInt(n.toInt)).opaque("integer")
    )
  def hex[_: P]: P[Expr] =
    positioned(
      P("0x" ~/ CharIn("0-9a-fA-F").rep(1)).!.map((n: String) =>
        EInt(Integer.parseInt(n.substring(2), 16), 16)
      ).opaque("hexademical")
    )
  def octal[_: P]: P[Expr] =
    positioned(
      P("0" ~ CharsWhileIn("0-7")).!.map((n: String) =>
        EInt(Integer.parseInt(n.substring(1), 8), 8)
      ).opaque("ocatal")
    )
  def rational[_: P]: P[Expr] =
    positioned(
      P(
        "-".? ~ ("0" | (CharIn("1-9") ~ CharsWhileIn("0-9").?)) ~
          "." ~/ CharsWhileIn("0-9")
      ).!.map(ERational(_)).opaque("rational")
    )
  def boolean[_: P]: P[Expr] =
    positioned(P(StringIn("true", "false")).!.map({
      case "true" => EBool(true)
      case "false" => EBool(false)
    }).opaque("boolean"))

  // Compound literals

  def recLitField[_: P]: P[(Id, Expr)] =
    P(iden ~ "=" ~/ expr)
      .map({
        case (id, e) => (id, e)
      })
      .opaque("<iden> = <expr>")
  def arrIn[_: P]: P[Expr] =
    positioned(P(expr.rep(1, sep = ",").map(es => EArrLiteral(es.toList))))
  def recIn[_: P]: P[Expr] =
    positioned(
      P(recLitField.rep(1, sep = ";").map(fs => ERecLiteral(fs.toMap)))
    )
  def compoundLiteral[_: P]: P[Expr] =
    positioned(P(braces(recIn | arrIn)).opaque("array or record literal"))

  // Access expressions
  def arrayAccess[_: P]: P[Expr] =
    positioned(P(iden ~ brackets(expr).rep(1)).map({
      case (id, idxs) => EArrAccess(id, idxs.toList)
    }))

  // Cast expressions or parenthesized expressions
  def exprCast[_: P]: P[Expr] =
    positioned(P(parens(expr ~ ("as" ~/ typAtom.opaque("type")).?)).map({
      case (e, Some(t)) => ECast(e, t)
      case (e, None) => e
    }))

  // Atoms that start with identifiers
  def appOrVar[_: P]: P[Expr] =
    positioned(P(iden ~/ parens(expr.rep(sep = ",")).?).map({
      case (f, Some(args)) => EApp(f, args.toList)
      case (id, None) => EVar(id)
    }))

  def simpleAtom[_: P]: P[Expr] = P(
    exprCast |
      compoundLiteral |
      arrayAccess |
      rational |
      hex |
      octal |
      uInt |
      boolean |
      appOrVar
  )

  // Record access syntax
  def recAccess[_: P]: P[Expr] =
    P((simpleAtom ~ ("." ~ iden).rep).map({
      case (rec, fields) =>
        fields.foldLeft[Expr](rec)({
          case (expr, field) => ERecAccess(expr, field)
        })
    }))

  // Binary operators
  import Syntax.{OpConstructor => OC}
  def mulOps[_: P]: P[BOp] =
    positioned(
      P(
        StringIn("/", "*", "%").!
      ).map({
        case "/" => NumOp("/", OC.div)
        case "*" => NumOp("*", OC.mul)
        case "%" => NumOp("%", OC.mod)
      })
    )
  def addOps[_: P]: P[BOp] =
    positioned(
      P(
        StringIn("+", "-").!
      ).map({
        case "+" => NumOp("+", OC.add)
        case "-" => NumOp("-", OC.sub)
      })
    )
  def eqOps[_: P]: P[BOp] =
    positioned(
      P(
        StringIn("==", "!=", ">=", "<=", ">", "<").!
      ).map({
        case op @ ("==" | "!=") => EqOp(op)
        case op @ (">=" | "<=" | ">" | "<") => CmpOp(op)
      })
    )
  def shOps[_: P]: P[BOp] =
    positioned(
      P(
        StringIn(">>", "<<").!
      ).map(op => BitOp(op))
    )
  def bAnd[_: P]: P[BOp] = positioned(P("&".!.map(op => BitOp(op))))
  def bOr[_: P]: P[BOp] = positioned(P("|".!.map(op => BitOp(op))))
  def bXor[_: P]: P[BOp] = positioned(P("^".!.map(op => BitOp(op))))
  def and[_: P]: P[BOp] = positioned(P("&&".!.map(op => BoolOp(op))))
  def or[_: P]: P[BOp] = positioned(P("||".!.map(op => BoolOp(op))))

  // Helper to generate binary op parsers
  def parseOp[_: P](atom: => P[Expr], op: => P[BOp]): P[Expr] =
    positioned({
      (atom ~ (op ~ atom).rep).map({
        case (left, rights) =>
          rights.foldLeft[Expr](left)({
            case (left, (op, right)) => EBinop(op, left, right)
          })
      })
    })

  def binMul[_: P]: P[Expr] = P(parseOp(recAccess, mulOps))
  def binAdd[_: P]: P[Expr] = P(parseOp(binMul, addOps))
  def binEq[_: P]: P[Expr] = P(parseOp(binAdd, eqOps))
  def binSh[_: P]: P[Expr] = P(parseOp(binEq, shOps))
  def binBAnd[_: P]: P[Expr] = P(parseOp(binSh, bAnd))
  def binBXor[_: P]: P[Expr] = P(parseOp(binBAnd, bXor))
  def binBOr[_: P]: P[Expr] = P(parseOp(binBXor, bOr))
  def binAnd[_: P]: P[Expr] = P(parseOp(binBOr, and))
  def binOr[_: P]: P[Expr] = P(parseOp(binAnd, or))

  def expr[_: P]: P[Expr] = binOr

  // For loops
  def range[_: P]: P[CRange] =
    positioned(
      P(
        parens(
          kw("let") ~/ iden ~ (":" ~ typ).? ~ "=" ~/ number ~/ ".." ~/ number
        )
          ~/ (kw("unroll") ~/ number).?
      ).map({
        case (id, typ, s, e, u) => CRange(id, typ, s, e, u.getOrElse(1))
      })
    )
  def cfor[_: P]: P[Command] =
    positioned(
      P(
        kw("for") ~/ range ~/ (kw("pipeline").!).? ~ block ~ (kw("combine") ~/ block).?
      ).map({
        case (range, pl, CBlock(par), Some(CBlock(c))) =>
          CFor(range, pl.isDefined, par, c)
        case (range, pl, CBlock(par), None) =>
          CFor(range, pl.isDefined, par, CEmpty)
        case _ =>
          throw CompilerError.Impossible(
            "Result of parsing a block command was not CBlock"
          )
      })
    )

  // While loops
  def whLoop[_: P]: P[Command] =
    positioned(
      P(kw("while") ~/ parens(expr) ~ kw("pipeline").!.? ~/ block).map({
        case (cond, pl, CBlock(body)) => CWhile(cond, pl.isDefined, body)
        case _ =>
          throw CompilerError.Impossible(
            "Result of parsing a block command was not CBlock"
          )
      })
    )

  // Conditionals
  def ifElse[_: P]: P[Command] =
    positioned(
      P(kw("if") ~/ parens(expr) ~ block ~ (kw("else") ~/ block).?).map({
        case (cond, CBlock(cons), Some(CBlock(alt))) =>
          CIf(cond, cons, alt)
        case (cond, CBlock(cons), None) =>
          CIf(cond, cons, CEmpty)
        case _ =>
          throw CompilerError.Impossible(
            "Result of parsing a block command was not CBlock"
          )
      })
    )

  // let
  def bind[_: P]: P[Command] =
    positioned((kw("let") ~/ iden ~ (":" ~ typ).? ~/ ("=" ~ expr).?).map({
      case (id, t, exp) => CLet(id, t, exp)
    }))

  // Update expressions
  def upd[_: P]: P[Command] =
    positioned(
      P(
        expr ~/ (
          ("=" ~/ Fail).opaque(
            "update statement. You probably meant to use `:=` instead of `=`."
          ) |
            StringIn(":=", "+=", "*=", "-=", "/=").! ~/ expr
        ).?
      ).map({
        case (l, Some((":=", r))) => CUpdate(l, r)
        case (l, Some((op, r))) => CReduce(ROp(op), l, r)
        case (l, None) => CExpr(l)
      })
    )

  // Views
  def viewSuffix[_: P]: P[Suffix] =
    positioned(
      P(
        "_".!.map(_ => Rotation(EInt(0))) |
          (number ~ "*" ~/ expr).map({ case (fac, e) => Aligned(fac, e) }) |
          (expr ~ "!").map(e => Rotation(e))
      ).opaque("<view-suffix>: _ | <number> * <expr> | <expr> !")
    )
  def viewParam[_: P]: P[View] =
    positioned(
      P(viewSuffix ~/ ":" ~ ("+" ~ number).? ~ (kw("bank") ~/ number).?).map({
        case (suf, prefixOpt, shrinkOpt) => View(suf, prefixOpt, shrinkOpt)
      })
    )
  def view[_: P]: P[Command] =
    positioned(
      P(kw("view") ~/ iden ~ "=" ~ iden ~ brackets(viewParam).rep(1)).map({
        case (id, arrId, params) => CView(id, arrId, params.toList)
      })
    )
  def split[_: P]: P[Command] =
    positioned(
      P(kw("split") ~/ iden ~ "=" ~ iden ~ brackets(kw("by") ~/ number).rep(1))
        .map({
          case (id, arrId, factors) => CSplit(id, arrId, factors.toList)
        })
    )

  def simpleCmd[_: P]: P[Command] = P(
    bind |
      view |
      split |
      positioned(kw("return") ~/ expr).map(e => CReturn(e)) |
      upd
  )

  // Block commands
  def block[_: P]: P[Command] = positioned(P(braces(cmd)).map(c => CBlock(c)))
  def blockCmd[_: P]: P[Command] = P(cfor | ifElse | whLoop | block | decor)

  def parCmd[_: P]: P[Command] =
    positioned(
      P(
        (blockCmd | (simpleCmd ~/ ";")).rep
      ).map(cmds => CPar.smart(cmds))
    )

  def cmd[_: P]: P[Command] =
    positioned(P(parCmd ~ ("---" ~/ parCmd).rep).map({
      case (init, rest) => CSeq.smart(init +: rest)
    }))

  // Functions
  def args[_: P]: P[Decl] =
    positioned(
      P(iden ~ ":" ~ typ)
        .map({ case (i, t) => Decl(i, t) })
        .opaque("<iden> : <typ>")
    )
  def retTyp[_: P]: P[Type] =
    positioned(P(":" ~ typ).?.map({
      case Some(t) => t
      case None => TVoid()
    }))
  def funcSignature[_: P]: P[FuncDef] =
    positioned(
      P(kw("def") ~/ iden ~ parens(args.rep(sep = ",")) ~ retTyp ~ ";").map({
        case (fn, args, ret) => FuncDef(fn, args.toList, ret, None)
      })
    )
  def funcDef[_: P]: P[FuncDef] =
    positioned(
      P(kw("def") ~/ iden ~ parens(args.rep(sep = ",")) ~ retTyp ~ "=" ~ block)
        .map({
          case (fn, args, ret, CBlock(body)) =>
            FuncDef(fn, args.toList, ret, Some(body))
          case _ =>
            throw CompilerError.Impossible(
              "Result of parsing a block command was not CBlock"
            )
        })
    )

  // Record definitions
  def recordDef[_: P]: P[RecordDef] =
    positioned(P(kw("record") ~/ iden ~ braces(args.rep(sep = ";"))).map({
      case (n, fs) => RecordDef(n, fs.map(d => d.id -> d.typ).toMap)
    }))

  // Declarations
  def decl[_: P]: P[Decl] =
    positioned(P(kw("decl") ~/ args ~ ";"))

  // include statements
  def include[_: P]: P[Include] =
    positioned(
      P(kw("import") ~/ stringVal ~ braces(funcSignature.rep))
        .map({
          case (name, funcs) => Include(name, funcs.toList)
        })
        .opaque("import <string> { <function signatures> }")
    )

  // Top-level decorations
  def decor[_: P]: P[CDecorate] =
    positioned(
      P(kw("decor") ~/ stringVal).map(CDecorate(_)).opaque("decor <string>")
    )

  def prog[_: P]: P[Prog] =
    positioned(
      P(
        include.rep.opaque("include statements") ~/
          (funcDef | recordDef).rep ~/
          decor.rep.opaque("top-level decors") ~/
          decl.rep.opaque("declarations") ~/
          cmd.? ~
          End
      ).map({
        case (incls, fns, decors, decls, cmd) =>
          Prog(
            incls.toList,
            fns.toList,
            decors.toList,
            decls.toList,
            cmd.getOrElse(CEmpty)
          )
      })
    )

  def parse(): Prog = {
    fastparse.parse[Prog](input, prog(_)) match {
      case Parsed.Success(e, _) => e
      case Parsed.Failure(_, index, extra) => {
        val traced = extra.trace()
        val loc = OffsetPosition(input, index)
        val msg = Errors.withPos(s"Expected ${traced.failure.label}", loc)

        throw Errors.ParserError(msg)
      }
      // XXX(rachit): Scala 2.13.4 complains that this pattern is not exhaustive.
      // This is not true...
      case _ => ???
    }
  }
}
