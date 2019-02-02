package fuselang

import scala.util.parsing.combinator._

import Syntax._

private class FuseParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  override protected val whiteSpace = """(\s|#.*|(/\*((\*[^/])|[^*])*\*/))+""".r

  val reservedTerms = Set("for", "if", "bit", "bool", "true", "false", "bank")

   // General parser combinators
  def braces[T](parser: P[T]): P[T] = "{" ~> parser <~ "}"
  def brackets[T](parser: P[T]): P[T] = "[" ~> parser <~ "]"
  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"
  def angular[T](parser: P[T]): P[T] = "<" ~> parser <~ ">"

  // General syntax components
  lazy val iden: P[String] = "" ~> "[a-z_][a-zA-Z0-9_]*".r

  // Atoms
  lazy val number = "(-)?[0-9]+".r ^^ { n => n.toInt }
  lazy val float = "(-)?[0-9]+.[0-9]+".r ^^ { n => n.toFloat }
  lazy val boolean = "true" ^^ { _ => true } | "false" ^^ { _ => false }
  lazy val eaa: P[Expr] = iden ~ rep1("[" ~> expr <~ "]") ^^ { case id ~ idxs => EAA(id, idxs) }
  lazy val atom: P[Expr] =
    eaa |
    float ^^ { case f => EFloat(f) } |
    number ^^ { case n => EInt(n) } |
    boolean ^^ { case b => EBool(b) } |
    iden ^^ { case id => EVar(id) }

  // Expressions
  lazy val binMul: P[Expr] =
    atom ~ ("/" ~> binMul) ^^ { case l ~ r => EBinop(OpDiv, l, r)} |
    atom ~ ("*" ~> binMul) ^^ { case l ~ r => EBinop(OpTimes, l, r)} |
    atom
  lazy val binAdd: P[Expr] =
    binMul ~ ("+" ~> binAdd) ^^ { case l ~ r => EBinop(OpAdd, l, r)} |
    binMul ~ ("-" ~> binAdd) ^^ { case l ~ r => EBinop(OpSub, l, r)} |
    binMul
  lazy val binEq: P[Expr] =
    binAdd ~ ("==" ~> binEq) ^^ { case l ~ r => EBinop(OpEq, l, r)} |
    binAdd ~ ("!=" ~> binEq) ^^ { case l ~ r => EBinop(OpNeq, l, r)} |
    binAdd ~ (">=" ~> binEq) ^^ { case l ~ r => EBinop(OpGte, l, r)} |
    binAdd ~ ("<=" ~> binEq) ^^ { case l ~ r => EBinop(OpLte, l, r)} |
    binAdd ~ (">" ~> binEq) ^^ { case l ~ r => EBinop(OpGt, l, r)} |
    binAdd ~ ("<" ~> binEq) ^^ { case l ~ r => EBinop(OpLt, l, r)} |
    binAdd
  lazy val expr = binEq | parens(binEq)

  // Types
  lazy val typIdx: P[(Int, Int)] =
    brackets(number ~ "bank" ~ number) ^^ { case n ~ _ ~ b => (n, b) } |
    brackets(number)^^ { n => (n, 1) }
  lazy val atyp: P[Type] =
    "float" ^^ { _ => TFloat } |
    "bool" ^^ { _ => TBool } |
    "bit" ~> angular(number) ^^ { case s => TSizedInt(s) }
  lazy val typ: P[Type] =
    atyp ~ rep1(typIdx) ^^ { case t ~ dims => TArray(t, dims) } |
    atyp

  // Commands
  lazy val block: P[Command] =
    braces(cmd) |
    "{" ~ "}" ^^ { case _ ~ _ => CEmpty }
  lazy val cfor: P[Command] =
    "for" ~ "(" ~ "let" ~> iden ~ "=" ~ number ~ ".." ~ number ~ ")" ~ block ^^ {
      case id ~ _ ~ s ~ _ ~ e ~ _ ~ par => CFor(id, CRange(s, e, 1), par, CReducer(CEmpty))
    } |
    "for" ~ "(" ~ "let" ~> iden ~ "=" ~ number ~ ".." ~ number ~ ")" ~ "unroll" ~ number ~ block ^^ {
      case id ~ _ ~ s ~ _ ~ e ~ _ ~ _ ~ u ~ par => CFor(id, CRange(s, e, u), par, CReducer(CEmpty))
    }
  lazy val acmd: P[Command] =
    cfor |
    "decl" ~> iden ~ ":" ~ typ ^^ { case id ~ _ ~ typ => CDecl(id, typ)} |
    expr ~ ":=" ~ expr ^^ { case l ~ _ ~ r => CUpdate(l, r) } |
    "let" ~> iden ~ "=" ~ expr ^^ { case id ~ _ ~ exp => CLet(id, exp) } |
    "if" ~> parens(expr) ~ block ^^ { case cond ~ cons => CIf(cond, cons) } |
    expr ^^ { case e => CExpr(e) }

  lazy val cmd: P[Command] =
    acmd ~ ";" ~ cmd ^^ { case c1 ~ _ ~ c2 => CSeq(c1, c2) } |
    acmd ~ ";" ~ "---" ~ cmd ^^ { case c1 ~ _ ~ _ ~ c2 => CSeq(c1, CSeq(CRefreshBanks, c2)) } |
    acmd <~ ";" |
    acmd

}

object FuseParser {
  private val parser = new FuseParser()
  import parser._

  def parse(str: String) = parseAll(cmd, str) match {
    case Success(res, _) => res
    case res => throw Errors.ParserError(s"$res")
  }
}

