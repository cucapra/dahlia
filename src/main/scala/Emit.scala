package fuselang

import org.bitbucket.inkytonik.kiama.output._

/**
 * This class aggressively uses Scala's implicitConversions. Make sure
 * that implicits classes never leak.
 * Implicit classes: https://docs.scala-lang.org/tour/implicit-conversions.html
 *
 * We also use the Kiama pretty printer combinators to generate the code.
 * For reference: https://bitbucket.org/inkytonik/kiama/src/master/wiki/PrettyPrinting.md?fileviewer=file-view-default
 */
private class Emit extends PrettyPrinter {

  import scala.language.implicitConversions
  import Syntax._
  import Errors._
  import CodeGenHelpers._

  override val defaultIndent = 2

  def unroll(n: Int) = n match {
    case 1 => ""
    case n => s"#pragma HLS UNROLL factor=$n"
  }

  def bank(id: Id, n: List[Int]): Doc = n.foldLeft(1)(_ * _) match {
    case 1 => value("")
    case n => value(s"#pragma HLS ARRAY_PARTITION variable=$id factor=$n")
  }

  // FIXME(rachit): This is probably incorrect.
  def flattenIdx(idxs: List[Expr], dimSizes: List[Int]) =
    idxs.zip(dimSizes).foldRight[Expr](EInt(0))({
      case ((idx, dim), acc) => idx + (EInt(dim) * acc)
    })

  def scope(doc: Doc): Doc =
    lbrace <@> indent(doc) <@> rbrace

  def emitBaseInt(v: Int, base: Int) = base match {
    case 8 => s"0${Integer.toString(v, 8)}"
    case 10 => v
    case 16 => s"0x${Integer.toString(v, 16)}"
  }

  implicit def IdToString(id: Id): Doc = value(id.v)

  implicit def typeToDoc(typ: Type): Doc = typ match {
    case _:TVoid => "void"
    case _:TBool | _:TIndex | _:TStaticInt => "int"
    case _:TFloat => "float"
    case _:TSizedInt => value(typ)
    case TArray(typ, _) => typ
    case TRecType(n, _) => n
    case _:TFun => throw Impossible("Cannot emit function types")
    case TAlias(n) => n
  }

  implicit def exprToDoc(e: Expr): Doc = e match {
    case EApp(fn, args) => fn <> parens(hsep(args.map(exprToDoc), comma))
    case EInt(v, base) => value(emitBaseInt(v, base))
    case EFloat(f) => value(f)
    case EBool(b) => value(if(b) 1 else 0)
    case EVar(id) => value(id)
    case EBinop(op, e1, e2) => parens(e1 <+> op.toString <+> e2)
    case EArrAccess(id, idxs) => id.typ match {
      case Some(TArray(_, dims)) => id <> brackets(flattenIdx(idxs, dims.map(_._1)))
      case Some(_) => throw Impossible("array access doesnt use array")
      case None => throw Impossible("type information missins in exprToDoc")
    }
    case ERecAccess(rec, field) => rec <> dot <> field
    case ERecLiteral(fs) => scope {
      hsep(fs.toList.map({ case (id, expr) => "." <> id <+> "=" <+> expr }), comma)
    }
  }

  implicit def cmdToDoc(c: Command): Doc = c match {
    case CPar(c1, c2) => c1 <@> c2
    case CSeq(c1, c2) => c1 <@> text("//---") <@> c2
    case CLet(id, typ, e) => typ.get <+> value(id) <+> equal <+> e <> semi
    case CIf(cond, cons, alt) =>
      "if" <> parens(cond) <> scope (cons) <+> "else" <> scope(alt)
    case CFor(range, par, combine) =>
      "for" <> parens {
        "int" <+> range.iter <+> "=" <+> value(range.s) <> semi <+>
        range.iter <+> "<" <+> value(range.e) <> semi <+>
        range.iter <> "++"
      } <+> scope {
        unroll(range.u) <@>
        par <@> text("// combiner:") <@>
        combine
      }
    case CWhile(cond, body) => "while" <> parens(cond) <+> scope(body)
    case CUpdate(lhs, rhs) => lhs <+> "=" <+> rhs <> semi
    case CReduce(rop, lhs, rhs) => lhs <+> rop.toString <+> rhs <> semi
    case CExpr(e) => e <> semi
    case CEmpty => ""
    case _:CView => throw Impossible("Views should not exist during codegen.")
  }

  def withArrayType(id: Id) = id.typ match {
    case Some(TArray(_, dims)) => s"$id[${dims.foldLeft(1)(_ * _._1).toString}]"
    case Some(_) => s"$id"
    case None =>
      throw Impossible("type information missing in withArrayType. Should not happen!")
  }
  // Declaration are the only place where arrays can be created, so use withArrayType
  // to generate the right array type.
  def declToDoc(d: Decl): Doc = d.typ <+> withArrayType(d.id)

  def bankPragmas(decls: List[Decl]) = decls
    .filter(d => d.typ.isInstanceOf[TArray])
    .map(d => d.id -> d.typ.asInstanceOf[TArray].dims.map(_._2))
    .map({ case (id, bfs) => bank(id, bfs) })

  def defToDoc(defi: Definition): Doc = defi match {
    case FuncDef(id, args, body) => {
      val as = hsep(args.map(declToDoc), comma)
      "void" <+> id <> parens(as) <+> scope {
        vsep(bankPragmas(args)) <@>
        body
      }
    }
    case RecordDef(name, fields) =>
      "typedef struct" <+> scope {
        vsep(fields.toList.map({ case (id, typ) => typ <+> id <> semi }))
      } <+> name <> semi
  }

  def progToDoc(p: Prog, c: Utils.Config) = {
    val pragmas = bankPragmas(p.decls)
    val args = hsep(p.decls.map(declToDoc), comma)

    vsep(p.defs.map(defToDoc)) <@>
    "void" <+> c.kernelName <> parens(args) <+> scope {
      vsep(pragmas) <@>
      p.cmd
    }
  }

  def emitProg(p: Prog, c: Utils.Config) =
    super.pretty(progToDoc(p, c)).layout
}

object Emit {
  private val emitter = new Emit()
  def emitProg = emitter.emitProg _
}
