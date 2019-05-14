package fuselang

import scala.util.parsing.combinator._

import Syntax._

private class FuseParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  override protected val whiteSpace = """(\s|\/\/.*|(/\*((\*[^/])|[^*])*\*/))+""".r

   // General parser combinators
  def braces[T](parser: P[T]): P[T] = "{" ~> parser <~ "}"
  def brackets[T](parser: P[T]): P[T] = "[" ~> parser <~ "]"
  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"
  def angular[T](parser: P[T]): P[T] = "<" ~> parser <~ ">"

  // General syntax components
  lazy val iden: P[Id] = positioned {
    "" ~> "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { v => Id(v) }
  }
  lazy val number = "[0-9]+".r ^^ { n => n.toInt } | err("Expected positive number")

  lazy val stringVal: P[String] =
    "\"" ~> "[^\"]*".r <~ "\""

  // Atoms
  lazy val uInt: P[Expr] = "[0-9]+".r ^^ { n => EInt(n.toInt) }
  lazy val hex = "0x[0-9a-fA-F]+".r ^^ { n => Integer.parseInt(n.substring(2), 16) }
  lazy val octal = "0[0-7]+".r ^^ { n => Integer.parseInt(n.substring(1), 8) }
  lazy val float = "(-)?[0-9]+\\.[0-9]+".r ^^ { n => n.toFloat }
  lazy val boolean = "true" ^^ { _ => true } | "false" ^^ { _ => false }

  lazy val eaa: P[Expr] = positioned {
    iden ~ rep1(brackets(expr)) ^^ { case id ~ idxs => EArrAccess(id, idxs) }
  }

  lazy val recLiteralField: P[(Id, Expr)] = iden ~ ("=" ~> expr) ^^ { case i ~ e => (i, e) }
  lazy val recLiteral: P[Expr] = positioned {
    braces(repsep(recLiteralField, ";")) ^^ { case fs => ERecLiteral(fs.toMap) }
  }

  lazy val exprCast: P[Expr] = parens(expr ~ "as" ~ atyp) ^^ { case e ~ _ ~ t => ECast(e, t)}

  lazy val simpleAtom: P[Expr] = positioned {
    eaa |
    recLiteral |
    float ^^ { case f => EFloat(f) } |
    hex ^^ { case h => EInt(h, 16) } |
    octal ^^ { case o => EInt(o, 8) } |
    uInt |
    boolean ^^ { case b => EBool(b) } |
    iden ~ parens(repsep(expr, ",")) ^^ { case f ~ args => EApp(f, args) } |
    iden ^^ { case id => EVar(id) } |
    exprCast |
    parens(expr)
  }

  lazy val recAccess: P[Expr] = positioned {
    recAccess ~ "." ~ iden ^^ { case rec ~ _ ~ f => ERecAccess(rec, f) } |
    simpleAtom
  }

  // Binops. Need to parse them seperately from EBinop to get positions.
  lazy val mulOps: P[BOp] = positioned {
    "/" ^^ { _ => OpDiv() } |
    "*" ^^ { _ => OpMul() } |
    "%" ^^ { _ => OpMul() }
  }
  lazy val addOps: P[BOp] = positioned {
    "+" ^^ { _ => OpAdd() } |
    "-" ^^ { _ => OpSub() }
  }
  lazy val eqOps: P[BOp] = positioned {
    "==" ^^ { _ => OpEq() } |
    "!=" ^^ { _ => OpNeq() } |
    ">=" ^^ { _ => OpGte() } |
    "<=" ^^ { _ => OpLte() } |
    ">"  ^^ { _ => OpGt() } |
    "<"  ^^ { _ => OpLt() }
  }
  lazy val shOps: P[BOp] = positioned {
    ">>" ^^ { _ => OpRsh()} |
    "<<" ^^ { _ => OpLsh()}
  }
  lazy val bAnd: P[BOp] = positioned("&" ^^ { _ => OpBAnd() })
  lazy val bOr: P[BOp] = positioned("|" ^^ { _ => OpBOr() })
  lazy val bXor: P[BOp] = positioned("^" ^^ { _ => OpBXor() })
  lazy val and: P[BOp] = positioned("&&" ^^ { _ => OpAnd() })
  lazy val or: P[BOp] = positioned("||" ^^ { _ => OpOr() })

  /** Expressions
   * The bin* parsers implement the precedence order of operators described
   * for C/C++: https://en.cppreference.com/w/c/language/operator_precedence
   * The tower-like structure is required to implement precedence correctly.
   */
  lazy val binMul: P[Expr] = positioned {
    recAccess ~ mulOps ~ binMul ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    recAccess
  }
  lazy val binAdd: P[Expr] = positioned {
    binMul ~ addOps ~ binAdd ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    binMul
  }
  lazy val binEq: P[Expr] = positioned {
    binAdd ~ eqOps ~ binEq ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    binAdd
  }
  lazy val binSh: P[Expr] = positioned {
    binEq ~ shOps ~ binSh ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    binEq
  }
  lazy val binBAnd: P[Expr] = positioned {
    binSh ~ bAnd ~ binBAnd ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    binSh
  }
  lazy val binBXor: P[Expr] = positioned {
    binBAnd ~ bXor ~ binBXor ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    binBAnd
  }
  lazy val binBOr: P[Expr] = positioned {
    binBXor ~ bOr ~ binBOr ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    binBXor
  }
  lazy val binAnd: P[Expr] = positioned {
    binBOr ~ and ~ binAnd ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    binBOr
  }
  lazy val binOr: P[Expr] = positioned {
    binAnd ~ or ~ binOr ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    binAnd
  }
  lazy val expr = positioned (binOr)

  // Types
  lazy val typIdx: P[(Int, Int)] =
    brackets(number ~ "bank" ~ number) ^^ { case n ~ _ ~ b => (n, b) } |
    brackets(number)^^ { n => (n, 1) }
  lazy val atyp: P[Type] =
    "float" ^^ { _ => TFloat() } |
    "double" ^^ { _ => TDouble() } |
    "bool" ^^ { _ => TBool() } |
    "bit" ~> angular(number) ^^ { case s => TSizedInt(s) } |
    iden ^^ { case id => TAlias(id) }
  lazy val typ: P[Type] =
    atyp ~ rep1(typIdx) ^^ { case t ~ dims => TArray(t, dims) } |
    atyp

  // For loops
  lazy val block: P[Command] =
    braces(cmd.?) ^^ { case c => c.getOrElse(CEmpty) }

  lazy val crange: P[CRange] = positioned {
    parens("let" ~> iden ~ "=" ~ number ~ ".." ~ number) ~ ("unroll" ~> number).? ^^ {
      case id ~ _ ~ s ~ _ ~ e ~ u => CRange(id, s, e, u.getOrElse(1))
    }
  }
  lazy val cfor: P[Command] = positioned {
    "for" ~> crange ~ block ~ ("combine" ~> block).? ^^ {
      case range ~ par ~ c => CFor(range, par, c.getOrElse(CEmpty))
    }
  }

  lazy val rop: P[ROp] = positioned {
    "+=" ^^ { _ => RAdd() } |
    "*=" ^^ { _ => RAdd() } |
    "-=" ^^ { _ => RAdd() } |
    "/=" ^^ { _ => RAdd() }
  }

  // Simple views
  lazy val viewSuffix: P[Suffix] = positioned {
    expr <~ "!" ^^ { case e => Rotation(e) } |
    number ~ "*" ~ expr ^^ { case fac ~ _ ~ e => Aligned(fac, e) } |
    "_" ^^ { case _ => Rotation(EInt(0)) }
  }

  lazy val viewParam: P[View] = positioned {
    viewSuffix ~ ":" ~ ("+" ~> number).? ~ ("bank" ~> number).? ^^ {
      case suf ~ _ ~ prefixOpt ~ shrinkOpt => View(suf, prefixOpt, shrinkOpt)
    }
  }

  lazy val view: P[Command] = positioned {
    "view" ~> iden ~ "=" ~ iden ~ rep1(brackets(viewParam)) ^^ {
      case id ~ _ ~ arrId ~ params => CView(id, arrId, params)
    }
  }

  // split views
  lazy val splitView: P[Command] = positioned {
    "split" ~> iden ~ "=" ~ iden ~ rep1(brackets("by" ~> number)) ^^ {
      case id ~ _ ~ arrId ~ factors => CSplit(id, arrId, factors)
    }
  }

  // If
  lazy val conditional: P[Command] = positioned {
    "if" ~> parens(expr) ~ block ~ ("else" ~> block).?  ^^ {
      case cond ~ cons ~ alt => CIf(cond, cons, if (alt.isDefined) alt.get else CEmpty )
    }
  }

  lazy val simpleCmd: P[Command] = positioned {
    "let" ~> iden ~ (":" ~> typ).? ~ ("=" ~> expr).? ^^ {
      case id ~ t ~ exp => CLet(id, t, exp)
    } |
    view | splitView |
    expr ~ ":=" ~ expr ^^ { case l ~ _ ~ r => CUpdate(l, r) } |
    expr ~ rop ~ expr ^^ { case l ~ rop ~ r => CReduce(rop, l, r) } |
    expr ^^ { case e => CExpr(e) }
  }

  lazy val blockCmd: P[Command] = positioned {
    block |
    cfor |
    conditional |
    "while" ~> parens(expr) ~ block ^^ { case cond ~ body => CWhile(cond, body) }
  }

  lazy val parCmd: P[Command] = positioned {
    simpleCmd ~ ";" ~ parCmd ^^ { case c1 ~ _ ~ c2 => CPar(c1, c2) } |
    blockCmd ~ parCmd ^^ { case c1 ~ c2 => CPar(c1, c2) } |
    simpleCmd <~ ";" | blockCmd | simpleCmd
  }

  lazy val cmd: P[Command] = positioned {
    parCmd ~ "---" ~ cmd ^^ { case c1 ~ _ ~ c2 => CSeq(c1, c2) } |
    parCmd
  }

  lazy val args: P[Decl] = iden ~ (":" ~> typ) ^^ { case i ~ t => Decl(i, t) }

  // Declarations
  lazy val decl: P[Decl] = positioned {
    "decl" ~> args  <~ ";"
  }

  // Definitions
  lazy val recordDef: P[RecordDef] = positioned {
    "record" ~> iden ~ braces(repsep(args, ";")) ^^ {
      case n ~ fs => RecordDef(n, fs.map(decl => decl.id -> decl.typ).toMap)
    }
  }
  lazy val externFuncDef: P[FuncDef] = positioned {
    "def" ~ "extern" ~> iden ~ parens(repsep(args, ",")) <~ ";" ^^ {
      case fn ~ args  => FuncDef(fn, args, None)
    }
  }
  lazy val funcDef: P[FuncDef] = positioned {
    externFuncDef |
    "def" ~> iden ~ parens(repsep(args, ",")) ~ block ^^ {
      case fn ~ args ~ body => FuncDef(fn, args, Some(body))
    }
  }
  lazy val defs = funcDef | recordDef

  // Include
  lazy val include: P[Include] = positioned {
    "import" ~> stringVal ~ braces(externFuncDef.*) ^^ {
      case name ~ funcs => Include(name, funcs)
    }
  }

  // Prog
  lazy val prog: P[Prog] = positioned {
    include.* ~ defs.* ~ decl.* ~ cmd.? ^^ {
      case incls ~ fns ~ decls ~ cmd =>
        Prog(incls, fns, decls, cmd.getOrElse(CEmpty))
    }
  }

}

object FuseParser {
  private val parser = new FuseParser()
  import parser._

  def parse(str: String): Prog = parseAll(prog, str) match {
    case Success(res, _) => res
    case res => throw Errors.ParserError(s"$res")
  }
}

