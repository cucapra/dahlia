package fuselang

import scala.util.parsing.combinator._

import fuselang.common._
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
  lazy val uInt: P[Expr] = "(-)?[0-9]+".r ^^ { n => EInt(n.toInt) }
  lazy val hex = "0x[0-9a-fA-F]+".r ^^ { n => Integer.parseInt(n.substring(2), 16) }
  lazy val octal = "0[0-7]+".r ^^ { n => Integer.parseInt(n.substring(1), 8) }
  lazy val double = "(-)?[0-9]+\\.[0-9]+".r ^^ {n => n.toDouble}
  lazy val boolean = "true" ^^ { _ => true } | "false" ^^ { _ => false }

  lazy val arrLiteral: P[Expr] = positioned {
    braces(repsep(expr, ",")) ^^ { case es => EArrLiteral(es) }
  }
  lazy val eaa: P[Expr] = positioned {
    iden ~ rep1(brackets(expr)) ^^ { case id ~ idxs => EArrAccess(id, idxs) }
  }

  lazy val recLiteralField: P[(Id, Expr)] = iden ~ ("=" ~> expr) ^^ { case i ~ e => (i, e) }
  lazy val recLiteral: P[Expr] = positioned {
    braces(repsep(recLiteralField, ";")) ^^ { case fs => ERecLiteral(fs.toMap) }
  }

  lazy val exprCast: P[Expr] = parens(expr ~ "as" ~ atyp) ^^ { case e ~ _ ~ t => ECast(e, t)}

  lazy val simpleAtom: P[Expr] = positioned {
    arrLiteral |
    eaa |
    recLiteral |
    double ^^ { case f => EDouble(f) } |
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
  import Syntax.{OpConstructor => OC}
  lazy val mulOps: P[BOp] = positioned {
    "/" ^^ { _ => NumOp("/", OC.div) } |
    "*" ^^ { _ => NumOp("*", OC.mul) } |
    "%" ^^ { _ => NumOp("%", OC.mod) }
  }
  lazy val addOps: P[BOp] = positioned {
    "+" ^^ { _ => NumOp("+", OC.add) } |
    "-" ^^ { _ => NumOp("-", OC.sub) }
  }
  lazy val eqOps: P[BOp] = positioned {
    ("==" | "!=") ^^ { op => EqOp(op) } |
    (">=" | "<=" | ">" | "<") ^^ { op => CmpOp(op) }
  }
  lazy val shOps: P[BOp] = positioned {
    (">>" | "<<") ^^ { op => BitOp(op)}
  }
  lazy val bAnd: P[BOp] = positioned("&" ^^ { op => BitOp(op) })
  lazy val bOr: P[BOp] = positioned("|" ^^  { op => BitOp(op) })
  lazy val bXor: P[BOp] = positioned("^" ^^ { op => BitOp(op) })

  lazy val and: P[BOp] = positioned("&&" ^^ { op => BoolOp(op) })
  lazy val or: P[BOp] = positioned("||" ^^  { op => BoolOp(op) })

  /** Expressions
   * The bin* parsers implement the precedence order of operators described
   * for C/C++: https://en.cppreference.com/w/c/language/operator_precedence
   * The tower-like structure is required to implement precedence correctly.
   */
  def parseOp(base: P[Expr], op: P[BOp]): P[Expr] = positioned {
    chainl1(base, op ^^ { case op => EBinop(op, _, _)})
  }
  lazy val binMul = parseOp(recAccess, mulOps)
  lazy val binAdd = parseOp(binMul, addOps)
  lazy val binEq = parseOp(binAdd, eqOps)
  lazy val binSh = parseOp(binEq, shOps)
  lazy val binBAnd = parseOp(binSh, bAnd)
  lazy val binBXor = parseOp(binBAnd, bXor)
  lazy val binBOr = parseOp(binBXor, bOr)
  lazy val binAnd = parseOp(binBOr, and)
  lazy val binOr = parseOp(binAnd, or)
  lazy val expr = positioned (binOr)

  // Types
  lazy val typIdx: P[(Int, Int)] =
    brackets(number ~ "bank" ~ number) ^^ { case n ~ _ ~ b => (n, b) } |
    brackets(number)^^ { n => (n, 1) }
  lazy val atyp: P[Type] =
    "float" ^^ { _ => TFloat() } |
    "double" ^^ { _ => TDouble() } |
    "bool" ^^ { _ => TBool() } |
    "bit" ~> angular(number~","~number ) ^^{ case s1~_~s2 => TSizedDouble(s1,s2,false) } |
    "bit" ~> angular(number) ^^{ case s => TSizedInt(s, false) } | 
    "ubit" ~> angular(number~","~number ) ^^{ case s1~_~s2 => TSizedDouble(s1,s2,true) } |
    "ubit" ~> angular(number) ^^{ case s => TSizedInt(s, true) } | 
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
    "for" ~> crange ~ "pipeline".? ~ block ~ ("combine" ~> block).? ^^ {
      case range ~ pl ~ par ~ c => CFor(range, pl.isDefined, par, c.getOrElse(CEmpty))
    }
  }

  lazy val rop: P[ROp] = positioned {
    ("+=" | "*=" | "-=" | "/=") ^^ { op => ROp(op) }
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
    "return" ~> expr ^^ { case e => CReturn(e) } |
    view | splitView |
    expr ~ ":=" ~ expr ^^ { case l ~ _ ~ r => CUpdate(l, r) } |
    expr ~ rop ~ expr ^^ { case l ~ rop ~ r => CReduce(rop, l, r) } |
    expr ^^ { case e => CExpr(e) }
  }

  lazy val blockCmd: P[Command] = positioned {
    block |
    cfor |
    conditional |
    "while" ~> parens(expr) ~ "pipeline".? ~ block ^^ { case cond ~ pl ~ body => CWhile(cond, pl.isDefined, body) } |
    decor
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
  lazy val retTyp: P[Type] = {
    (":" ~> typ).? ^^ {
      case Some(t) => t
      case None => TVoid()
    }
  }
  lazy val funcDef: P[FuncDef] = positioned {
    "def" ~> iden ~ parens(repsep(args, ",")) ~ retTyp ~ block ^^ {
      case fn ~ args ~ ret ~ body => FuncDef(fn, args, ret, Some(body))
    }
  }
  lazy val defs = funcDef | recordDef

  // Include
  lazy val externFuncDef: P[FuncDef] = positioned {
    "def" ~> iden ~ parens(repsep(args, ",")) ~ retTyp <~ ";" ^^ {
      case fn ~ args ~ ret => FuncDef(fn, args, ret, None)
    }
  }
  lazy val include: P[Include] = positioned {
    "import" ~> stringVal ~ braces(externFuncDef.*) ^^ {
      case name ~ funcs => Include(name, funcs)
    }
  }

  // Top-level decorations (for the kernel function).
  lazy val decor: P[CDecorate] = {
    "decor" ~> stringVal ^^ { case value => CDecorate(value) }
  }

  // Prog
  lazy val prog: P[Prog] = positioned {
    include.* ~ defs.* ~ decor.* ~ decl.* ~ cmd.? ^^ {
      case incls ~ fns ~ decors ~ decls ~ cmd =>
        Prog(incls, fns, decors, decls, cmd.getOrElse(CEmpty))
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

