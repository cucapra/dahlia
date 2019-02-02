package fuselang

import org.bitbucket.inkytonik.kiama.output._

/**
 * This class is aggressively using Scala's implicitConversions. Make sure
 * that implicits classes never leak.
 *
 * Implicit classes: https://docs.scala-lang.org/tour/implicit-conversions.html
 */
private class Emit extends PrettyPrinter {

  import scala.language.implicitConversions
  import Syntax._
  import TypeEnv.Env

  override val defaultIndent = 2

  def scope(doc: Doc): Doc =
    lbrace <@> indent(doc) <@> rbrace

  implicit def typeToDoc(typ: Type): Doc = typ match {
    case TBool | TIndex(_, _) | TStaticInt(_) => "int"
    case TFloat => "float"
    case TSizedInt(_) => value(typ)
    case TArray(typ, dims) => typ <> brackets(value(dims.map(_._1).foldLeft(1)(_ * _)))
  }

  implicit def exprToDoc(e: Expr): Doc = e match {
    case EInt(i) => value(i)
    case EFloat(f) => value(f)
    case EBool(b) => value(if(b) 1 else 0)
    case EVar(id) => value(id)
    case EBinop(op, e1, e2) => e1 <+> op.toString <+> e2
    case EAA(id, idxs) => id <> hcat(idxs.map(idx => brackets(idx)))
  }

  implicit def cmdToDoc(c: Command)(implicit env: Env): Doc = c match {
    case CDecl(id, typ) => typ <+> id <> semi
    case CSeq(c1, c2) => c1 <> line <> c2
    case CLet(id, e) => env(id).typ <+> value(id) <+> equal <+> e <> semi
    case CIf(cond, cons) => "if" <> parens(cond) <> scope (cons)
    case CFor(iter, range, par, CReducer(reduce)) =>
      "for" <> parens {
        env(iter).typ <+> iter <+> "=" <+> value(range.s) <> semi <+>
        iter <+> "<" <+> value(range.e) <> semi <+>
        iter <+> "++"
      } <+> scope(par <> line <> text("// reducer:") <> reduce)
    case CUpdate(lhs, rhs) => lhs <+> "=" <+> rhs <> semi
    case CExpr(e) => e <> semi
    case CEmpty => ""
    case CRefreshBanks => "//---"
  }

  def emitC(c: Command, env: Env) =
    super.pretty(cmdToDoc(c)(env)).layout

}

object Emit {
  private val emitter = new Emit()
  def emitC = emitter.emitC _
}
