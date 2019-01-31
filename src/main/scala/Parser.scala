package fuselang

import scala.util.parsing.combinator._

import Syntax._

private class FuseParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  override protected val whiteSpace = """(\s|#.*|(/\*((\*[^/])|[^*])*\*/))+""".r

   // General parser combinators
  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"
  def braces[T](parser: P[T]): P[T] = "{" ~> parser <~ "}"

  // General syntax components
  lazy val iden: P[String] = "" ~> "[a-z_][a-zA-Z0-9_]*".r

  // Base constants
  lazy val number: P[Int] = "(-)?[0-9]+".r ^^ { n => Integer.parseInt(n) }
  lazy val boolean: P[Boolean] = "true" ^^ { _ => true } | "false" ^^ { _ => false }

  lazy val idx: P[Expr] =
    "[" ~> expr <~ "]" ^^ { e => e }
  lazy val eaa: P[Expr] = iden ~ rep1(idx) ^^ { case id ~ idxs => EAA(id, idxs) }

  lazy val atom: P[Expr] =
    eaa |
    number ^^ { case n => EInt(n) } |
    boolean ^^ { case b => EBool(b) } |
    iden ^^ { case id => EVar(id) }

  lazy val binAdd: P[Expr] =
    atom ~ ("+" ~> binAdd) ^^ { case l ~ r => EBinop(OpAdd, l, r)} |
    atom

  lazy val binEq: P[Expr] =
    binAdd ~ ("==" ~> binEq) ^^ { case l ~ r => EBinop(OpEq, l, r)} |
    binAdd

  lazy val expr = binEq

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
    expr ~ ":=" ~ expr ^^ { case l ~ _ ~ r => CUpdate(l, r) } |
    "let" ~> iden ~ "=" ~ expr ^^ { case id ~ _ ~ exp => CLet(id, exp) } |
    "if" ~ "(" ~> expr ~ ")" ~ block ^^ { case cond ~ _ ~ cons => CIf(cond, cons) } |
    expr ^^ { case e => CExpr(e) }

  lazy val cmd: P[Command] =
    acmd ~ ";" ~ cmd ^^ { case c1 ~ _ ~ c2 => CSeq(c1, c2) } |
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

