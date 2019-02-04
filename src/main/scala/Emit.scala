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

  override val defaultIndent = 2

  def unroll(n: Int) = n match {
    case 1 => ""
    case n => s"#pragma HLS UNROLL factor=$n"
  }

  def bank(id: Id, n: List[Int]): Doc = n.foldLeft(1)(_ * _) match {
    case 1 => value("")
    case n => value(s"#pragma HLS ARRAY_PARTITION variable=$id factor=$n")
  }

  def scope(doc: Doc): Doc =
    lbrace <@> indent(doc) <@> rbrace

  implicit def IdToString(id: Id): Doc = value(id.v)

  implicit def typeToDoc(typ: Type): Doc = typ match {
    case _: TBool | TIndex(_, _) | TStaticInt(_) => "int"
    case _: TFloat => "float"
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

  implicit def cmdToDoc(c: Command): Doc = c match {
    case CSeq(c1, c2) => c1 <> line <> c2
    case CLet(id, typ, e) => typ.get <+> value(id) <+> equal <+> e <> semi
    case CIf(cond, cons) => "if" <> parens(cond) <> scope (cons)
    case CFor(range, par, combine) =>
      "for" <> parens {
        "int" <+> range.iter <+> "=" <+> value(range.s) <> semi <+>
        range.iter <+> "<" <+> value(range.e) <> semi <+>
        range.iter <+> "++"
      } <+> scope {
        unroll(range.u) <@>
        par <> line <> text("// combiner:") <@>
        combine
      }
    case CUpdate(lhs, rhs) => lhs <+> "=" <+> rhs <> semi
    case CReduce(rop, lhs, rhs) => lhs <+> rop.toString <+> rhs <> semi
    case CExpr(e) => e <> semi
    case CEmpty => ""
    case CRefreshBanks() => "//---"
  }

  def declToDoc(d: Decl): Doc = d.typ <+> d.id <> semi

  def progToDoc(p: Prog) = {
    val bankPragmas = p.decls
      .filter(d => d.typ.isInstanceOf[TArray])
      .map(d => d.id -> d.typ.asInstanceOf[TArray].dims.map(_._1))
      .map({ case (id, bfs) => bank(id, bfs) })
    val args = hsep(p.decls.map(declToDoc), comma)
    "void kernel" <> parens(args) <+> scope {
      vsep(bankPragmas) <@>
      p.cmd
    }
  }

  def emitProg(p: Prog) =
    super.pretty(progToDoc(p)).layout

}

object Emit {
  private val emitter = new Emit()
  def emitProg = emitter.emitProg _
}
