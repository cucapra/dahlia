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

  // Atoms
  lazy val number = "(-)?[0-9]+".r ^^ { n => n.toInt }
  lazy val float = "(-)?[0-9]+.[0-9]+".r ^^ { n => n.toFloat }
  lazy val boolean = "true" ^^ { _ => true } | "false" ^^ { _ => false }
  lazy val eaa: P[Expr] = positioned {
    iden ~ rep1(brackets(expr)) ^^ { case id ~ idxs => EAA(id, idxs) }
  }
  lazy val atom: P[Expr] = positioned {
    eaa |
    float ^^ { case f => EFloat(f) } |
    number ^^ { case n => EInt(n) } |
    boolean ^^ { case b => EBool(b) } |
    iden ~ parens(repsep(expr, ",")) ^^ { case f ~ args => EApp(f, args) } |
    iden ^^ { case id => EVar(id) } |
    parens(expr)
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

  // Expressions
  // The bin* parsers implement the precedence order of operators described
  // for C/C++: https://en.cppreference.com/w/c/language/operator_precedence
  // The tower-like structure is required to implement precedence correctly.
  lazy val binMul: P[Expr] = positioned {
    atom ~ mulOps ~ binMul ^^ { case l ~ op ~ r => EBinop(op, l, r)} |
    atom
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
    "bool" ^^ { _ => TBool() } |
    "bit" ~> angular(number) ^^ { case s => TSizedInt(s) }
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

  // Some ugly validation for shrink step size = width
  lazy val viewParam: P[(Expr, Int, Int)] =
    number ~ "*" ~ expr ~ ":" ~ number ^^ {
      case step ~ _ ~ idx ~ _ ~ width => (idx, width, step)
    } |
    number ~ ":" ~ number ^^ {
      case idx ~ _ ~ width => (EInt(idx), width, width)
    }

  lazy val view: P[ViewType] = positioned {
    "shrink" ~> iden ~ rep1(brackets(viewParam)) ^^ {
      case arrId ~ params => Shrink(arrId, params)
    }
  }

  // Other commands
  lazy val acmd: P[Command] = positioned {
    block |
    cfor |
    "let" ~> iden ~ (":" ~> typ).? ~ ("=" ~> expr) ^^ { case id ~ t ~ exp => CLet(id, t, exp) } |
    "view" ~> iden ~ "=" ~ view ^^ { case arrId ~ _ ~ vt => CView(arrId, vt) } |
    "if" ~> parens(expr) ~ block  ^^ { case cond ~ cons => CIf(cond, cons) } |
    expr ~ ":=" ~ expr ^^ { case l ~ _ ~ r => CUpdate(l, r) } |
    expr ~ rop ~ expr ^^ { case l ~ rop ~ r => CReduce(rop, l, r) } |
    expr ^^ { case e => CExpr(e) }
  }

  lazy val scmd: P[Command] = positioned {
    acmd ~ ";" ~ scmd ^^ { case c1 ~ _ ~ c2 => CPar(c1, c2) } |
    acmd <~ ";" | acmd
  }

  lazy val cmd: P[Command] = positioned {
    scmd ~ "---" ~ cmd ^^ { case c1 ~ _ ~ c2 => CSeq(c1, c2) } |
    scmd
  }

  lazy val args: P[Decl] = iden ~ (":" ~> typ) ^^ { case i ~ t => Decl(i, t) }

  // Declarations
  lazy val decl: P[Decl] = positioned {
    "decl" ~> args  <~ ";"
  }

  // Functions
  lazy val fDef: P[FDef] = positioned {
    "def" ~> iden ~ parens(repsep(args, ",")) ~ block ^^ {
      case fn ~ args ~ body => FDef(fn, args, body)
    }
  }

  // Prog
  lazy val prog: P[Prog] = positioned {
    fDef.* ~ decl.* ~ cmd.? ^^ {
      case fns ~ decls ~ cmd => Prog(fns, decls, cmd.getOrElse(CEmpty))
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

