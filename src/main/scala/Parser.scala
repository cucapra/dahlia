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
  lazy val number: P[Int] = "(-)?[0-9]+".r ^^ { n => Integer.parseInt(n) }
  lazy val boolean: P[Boolean] = "true" ^^ { _ => true } | "false" ^^ { _ => false }
  lazy val eaa: P[Expr] = iden ~ rep1("[" ~> expr <~ "]") ^^ { case id ~ idxs => EAA(id, idxs) }
  lazy val atom: P[Expr] =
    eaa |
    number ^^ { case n => EInt(n) } |
    boolean ^^ { case b => EBool(b) } |
    iden ^^ { case id => EVar(id) }

  // Expressions
  lazy val binAdd: P[Expr] =
    atom ~ ("+" ~> binAdd) ^^ { case l ~ r => EBinop(OpAdd, l, r)} |
    atom
  lazy val binEq: P[Expr] =
    binAdd ~ ("==" ~> binEq) ^^ { case l ~ r => EBinop(OpEq, l, r)} |
    binAdd
  lazy val expr = binEq

  // Types
  lazy val typIdx: P[(Int, Int)] =
    brackets(number ~ "bank" ~ number) ^^ { case n ~ _ ~ b => (n, b) } |
    brackets(number)^^ { n => (n, 1) }
  lazy val atyp: P[Type] =
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
      case id ~ _ ~ s ~ _ ~ e ~ _ ~ par => CFor(id, CRange(s, e, 1), par)
    } |
    "for" ~ "(" ~ "let" ~> iden ~ "=" ~ number ~ ".." ~ number ~ ")" ~ "unroll" ~ number ~ block ^^ {
      case id ~ _ ~ s ~ _ ~ e ~ _ ~ _ ~ u ~ par => CFor(id, CRange(s, e, u), par)
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
    case res => throw new RuntimeException(s"$res")
  }
}

