package fuselang

import scala.util.parsing.input.OffsetPosition

import fastparse._
import fastparse.JavaWhitespace._

import fuselang.common._
import Syntax._
import Configuration.stringToBackend
import Utils.RichOption
import CompilerError.BackendError

case class Parser(input: String) {

  // Common surround expressions
  def braces[K: P, T](p: => P[T]): P[T] = P("{" ~/ p ~ "}")
  def brackets[K: P, T](p: => P[T]): P[T] = P("[" ~ p ~ "]")
  def angular[K: P, T](p: => P[T]): P[T] = P("<" ~/ p ~ ">")
  def parens[K: P, T](p: => P[T]): P[T] = P("(" ~/ p ~ ")")

  def positioned[K: P, T <: PositionalWithSpan](p: => P[T]): P[T] = {
    P(Index ~ p ~ Index).map({
      case (index, t, end) => {
        val startPos = OffsetPosition(input, index)
        val out = t.setPos(startPos)
        val endPos = OffsetPosition(input, end)
        if (startPos.line == endPos.line) {
          out.setSpan(end - index)
        }
        out
      }
    })
  }

  /*def notKws[K: P] = {
    import fastparse.NoWhitespace._
    P(!(StringIn(
      "float", "double", "bool", "bit", "ubit", "fix", "ufix", "let", "for",
      "bank", "unroll", "decl", "true", "false", "as", "pipeline", "combine",
      "if", "else", "import", "decor", "def", "record", "split", "view",
      "return", "while"
    ) ~ &(" "))).opaque("non reserved keywords")
  }*/

  def kw[K: P](word: String): P[Unit] = {
    import fastparse.NoWhitespace._
    P(word ~ !CharsWhileIn("a-zA-Z0-9_"))
  }

  // Basic atoms
  def iden[K: P]: P[Id] = {
    import fastparse.NoWhitespace._
    positioned(P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").?).!.map({
      case rest => Id(rest)
    }).opaque("Expected valid identifier"))
  }

  def number[K: P]: P[Int] =
    P(CharIn("0-9").rep(1).!.map(_.toInt)).opaque("Expected positive number")

  def stringVal[K: P]: P[String] =
    P("\"" ~/ CharPred(_ != '"').rep.! ~ "\"")

  // Types
  def typAtom[K: P]: P[Type] =
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

  def typIdx[K: P]: P[DimSpec] =
    P(brackets(number ~ (kw("bank") ~ number).?)).map({
      case (n, b) => (n, b.getOrElse(1))
    })

  def typ[K: P]: P[Type] =
    positioned(P(typAtom ~ (braces(number).? ~ typIdx.rep(1)).?).map({
      case (typ, Some((ports, dims))) =>
        TArray(typ, dims.toList, ports.getOrElse(1))
      case (typ, None) => typ
    }))

  // Literals
  def uInt[K: P]: P[Expr] =
    positioned(
      P(
        "0" | "-".? ~ (CharIn("1-9") ~ CharsWhileIn("0-9").?)
      ).!.map((n: String) => EInt(BigInt(n))).opaque("integer")
    )
  def hex[K: P]: P[Expr] =
    positioned(
      P("0x" ~/ CharsWhileIn("0-9a-fA-F")).!.map((n: String) =>
        EInt(BigInt(n.substring(2), 16), 16)
      ).opaque("hexademical")
    )
  def octal[K: P]: P[Expr] =
    positioned(
      P("0" ~ CharsWhileIn("0-7")).!.map((n: String) =>
        EInt(BigInt(n.substring(1), 8), 8)
      ).opaque("ocatal")
    )
  def rational[K: P]: P[Expr] =
    positioned(
      P(
        "-".? ~ ("0" | (CharIn("1-9") ~ CharsWhileIn("0-9").?)) ~
          "." ~/ CharsWhileIn("0-9")
      ).!.map(ERational(_)).opaque("rational")
    )
  def boolean[K: P]: P[Expr] =
    positioned(P(StringIn("true", "false")).!.map({
      case "true" => EBool(true)
      case "false" => EBool(false)
    }).opaque("boolean"))

  // Compound literals

  def recLitField[K: P]: P[(Id, Expr)] =
    P(iden ~ "=" ~/ expr)
      .map({
        case (id, e) => (id, e)
      })
      .opaque("<iden> = <expr>")
  def arrIn[K: P]: P[Expr] =
    positioned(P(expr.rep(1, sep = ",").map(es => EArrLiteral(es.toList))))
  def recIn[K: P]: P[Expr] =
    positioned(
      P(recLitField.rep(1, sep = ";").map(fs => ERecLiteral(fs.toMap)))
    )
  def compoundLiteral[K: P]: P[Expr] =
    positioned(P(braces(recIn | arrIn)).opaque("array or record literal"))

  // Access expressions
  def arrayAccess[K: P]: P[Expr] =
    positioned(P(iden ~ brackets(expr).rep(1)).map({
      case (id, idxs) => EArrAccess(id, idxs.toList)
    }))

  // Cast expressions or parenthesized expressions
  def exprCast[K: P]: P[Expr] =
    positioned(P(parens(expr ~ ("as" ~/ typAtom.opaque("type")).?)).map({
      case (e, Some(t)) => ECast(e, t)
      case (e, None) => e
    }))

  // Atoms that start with identifiers
  def appOrVar[K: P]: P[Expr] =
    positioned(P(iden ~/ parens(expr.rep(sep = ",")).?).map({
      case (f, Some(args)) => EApp(f, args.toList)
      case (id, None) => EVar(id)
    }))

  def simpleAtom[K: P]: P[Expr] = P(
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
  def recAccess[K: P]: P[Expr] =
    P((simpleAtom ~ ("." ~ iden).rep).map({
      case (rec, fields) =>
        fields.foldLeft[Expr](rec)({
          case (expr, field) => ERecAccess(expr, field)
        })
    }))

  // Binary operators
  import Syntax.{OpConstructor => OC}
  def mulOps[K: P]: P[BOp] =
    positioned(
      P(
        StringIn("/", "*", "%").!
      ).map({
        case "/" => NumOp("/", OC.div)
        case "*" => NumOp("*", OC.mul)
        case "%" => NumOp("%", OC.mod)
      })
    )
  def addOps[K: P]: P[BOp] =
    positioned(
      P(
        StringIn("+", "-").!
      ).map({
        case "+" => NumOp("+", OC.add)
        case "-" => NumOp("-", OC.sub)
      })
    )
  def eqOps[K: P]: P[BOp] =
    positioned(
      P(
        StringIn("==", "!=", ">=", "<=", ">", "<").!
      ).map({
        case op @ ("==" | "!=") => EqOp(op)
        case op @ (">=" | "<=" | ">" | "<") => CmpOp(op)
      })
    )
  def shOps[K: P]: P[BOp] =
    positioned(
      P(
        StringIn(">>", "<<").!
      ).map(op => BitOp(op))
    )
  def bAnd[K: P]: P[BOp] = positioned(P("&".!.map(op => BitOp(op))))
  def bOr[K: P]: P[BOp] = positioned(P("|".!.map(op => BitOp(op))))
  def bXor[K: P]: P[BOp] = positioned(P("^".!.map(op => BitOp(op))))
  def and[K: P]: P[BOp] = positioned(P("&&".!.map(op => BoolOp(op))))
  def or[K: P]: P[BOp] = positioned(P("||".!.map(op => BoolOp(op))))

  // Helper to generate binary op parsers
  def parseOp[K: P](atom: => P[Expr], op: => P[BOp]): P[Expr] =
    positioned({
      (atom ~ (op ~ atom).rep).map({
        case (left, rights) =>
          rights.foldLeft[Expr](left)({
            case (left, (op, right)) => EBinop(op, left, right)
          })
      })
    })

  def binMul[K: P]: P[Expr] = P(parseOp(recAccess, mulOps))
  def binAdd[K: P]: P[Expr] = P(parseOp(binMul, addOps))
  def binEq[K: P]: P[Expr] = P(parseOp(binAdd, eqOps))
  def binSh[K: P]: P[Expr] = P(parseOp(binEq, shOps))
  def binBAnd[K: P]: P[Expr] = P(parseOp(binSh, bAnd))
  def binBXor[K: P]: P[Expr] = P(parseOp(binBAnd, bXor))
  def binBOr[K: P]: P[Expr] = P(parseOp(binBXor, bOr))
  def binAnd[K: P]: P[Expr] = P(parseOp(binBOr, and))
  def binOr[K: P]: P[Expr] = P(parseOp(binAnd, or))

  def expr[K: P]: P[Expr] = binOr

  // For loops
  def range[K: P]: P[CRange] =
    positioned(
      P(
        parens(
          kw("let") ~/ iden ~ (":" ~ typ).? ~ "=" ~ ("rev").!.? ~/ number ~/ ".." ~/ number
        )
          ~/ (kw("unroll") ~/ number).?
      ).map({
        case (id, typ, rev, s, e, u) =>
          CRange(id, typ, rev.isDefined, s, e, u.getOrElse(1))
      })
    )
  def cfor[K: P]: P[Command] =
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
  def whLoop[K: P]: P[Command] =
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
  def ifElse[K: P]: P[Command] =
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
  def bind[K: P]: P[Command] =
    positioned((kw("let") ~/ iden ~ (":" ~ typ).? ~/ ("=" ~ expr).?).map({
      case (id, t, exp) => CLet(id, t, exp)
    }))

  // Update expressions
  def upd[K: P]: P[Command] =
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
  def viewSuffix[K: P]: P[Suffix] =
    positioned(
      P(
        "_".!.map(_ => Rotation(EInt(0))) |
          (number ~ "*" ~/ expr).map({ case (fac, e) => Aligned(fac, e) }) |
          (expr ~ "!").map(e => Rotation(e))
      ).opaque("<view-suffix>: _ | <number> * <expr> | <expr> !")
    )
  def viewParam[K: P]: P[View] =
    positioned(
      P(viewSuffix ~/ ":" ~ ("+" ~ number).? ~ (kw("bank") ~/ number).?).map({
        case (suf, prefixOpt, shrinkOpt) => View(suf, prefixOpt, shrinkOpt)
      })
    )
  def view[K: P]: P[Command] =
    positioned(
      P(kw("view") ~/ iden ~ "=" ~ iden ~ brackets(viewParam).rep(1)).map({
        case (id, arrId, params) => CView(id, arrId, params.toList)
      })
    )
  def split[K: P]: P[Command] =
    positioned(
      P(kw("split") ~/ iden ~ "=" ~ iden ~ brackets(kw("by") ~/ number).rep(1))
        .map({
          case (id, arrId, factors) => CSplit(id, arrId, factors.toList)
        })
    )

  def simpleCmd[K: P]: P[Command] = P(
    bind |
      view |
      split |
      positioned(kw("return") ~/ expr).map(e => CReturn(e)) |
      upd
  )

  // Block commands
  def block[K: P]: P[Command] = positioned(P(braces(cmd)).map(c => CBlock(c)))
  def blockCmd[K: P]: P[Command] = P(cfor | ifElse | whLoop | block | decor)

  def parCmd[K: P]: P[Command] =
    positioned(
      P(
        (blockCmd | (simpleCmd ~/ ";")).rep
      ).map(cmds => CPar.smart(cmds))
    )

  def cmd[K: P]: P[Command] =
    positioned(P(parCmd ~ ("---" ~/ parCmd).rep).map({
      case (init, rest) => CSeq.smart(init +: rest)
    }))

  // Functions
  def args[K: P]: P[Decl] =
    positioned(
      P(iden ~ ":" ~ typ)
        .map({ case (i, t) => Decl(i, t) })
        .opaque("<iden> : <typ>")
    )
  def retTyp[K: P]: P[Type] =
    positioned(P(":" ~ typ).?.map({
      case Some(t) => t
      case None => TVoid()
    }))
  def funcSignature[K: P]: P[FuncDef] =
    positioned(
      P(kw("def") ~/ iden ~ parens(args.rep(sep = ",")) ~ retTyp ~ ";").map({
        case (fn, args, ret) => FuncDef(fn, args.toList, ret, None)
      })
    )
  def funcDef[K: P]: P[FuncDef] =
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
  def recordDef[K: P]: P[RecordDef] =
    positioned(P(kw("record") ~/ iden ~ braces(args.rep(sep = ";"))).map({
      case (n, fs) => RecordDef(n, fs.map(d => d.id -> d.typ).toMap)
    }))

  // Declarations
  def decl[K: P]: P[Decl] =
    positioned(P(kw("decl") ~/ args ~ ";"))

  def backend[K: P]: P[String] =
    P(("c++" | "vivado" | "futil" | "calyx").!)
      .map(s => s)
      .opaque("known backend: `vivado(\"...\")`")

  // include statements
  def include[K: P]: P[Include] =
    positioned(
      P(kw("import") ~/
        (backend ~/ parens(stringVal)).rep(1) ~/
        braces(funcSignature.rep)
      ).map({
        case (backends, funcs) => {
          val bs = backends.map({ case (b, imp) =>
            stringToBackend(b).getOrThrow(BackendError(s"Unknown backend: $b")) -> imp
          }).toMap
          Include(bs, funcs.toList)
        }
        })
    )

  // Top-level decorations
  def decor[K: P]: P[CDecorate] =
    positioned(
      P(kw("decor") ~/ stringVal).map(CDecorate(_)).opaque("decor <string>")
    )

  def prog[K: P]: P[Prog] =
    positioned(
      P(
        include.rep ~/
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
