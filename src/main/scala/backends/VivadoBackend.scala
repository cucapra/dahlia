package fuselang.backend

import Cpp._
import PrettyPrint.Doc
import PrettyPrint.Doc._

import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._

private class VivadoBackend extends CppLike {
  val CppPreamble: Doc = """
    |#include <ap_int.h>
  """.stripMargin.trim

  def unroll(n: Int): Doc = n match {
    case 1 => emptyDoc
    case n => value(s"#pragma HLS UNROLL factor=$n skip_exit_check") <@> line
  }

  def bank(id: Id, banks: List[Int]): String = banks.zipWithIndex.foldLeft(""){
    case (acc, (bank, dim)) =>
      if (bank != 1) {
        s"${acc}#pragma HLS ARRAY_PARTITION variable=$id cyclic factor=$bank dim=${dim + 1}"
      } else {
        acc
      }
  }

  def bankPragmas(decls: List[Decl]): List[Doc] = decls
    .collect({ case Decl(id, typ: TArray) => bank(id, typ.dims.map(_._2)) })
    .withFilter(s => s != "")
    .map(s => value(s))

  override def emitLet(let: CLet): Doc = {
    super.emitLet(let) <@>
    (let.typ match {
      case Some(t) => vsep(bankPragmas(List(Decl(let.id, t))))
      case None => emptyDoc
    })
  }

  def emitPipeline(enabled: Boolean): Doc =
    if (enabled) value(s"#pragma HLS PIPELINE") <> line else emptyDoc

  def emitFor(cmd: CFor): Doc =
    text("for") <> emitRange(cmd.range) <+> scope {
      emitPipeline(cmd.pipeline) <>
      unroll(cmd.range.u) <>
      cmd.par <>
      (if (cmd.combine != CEmpty) line <> text("// combiner:") <@> cmd.combine
       else emptyDoc)
    }

  override def emitWhile(cmd: CWhile): Doc =
      text("while") <> parens(cmd.cond) <+> scope {
        emitPipeline(cmd.pipeline) <>
        cmd.body
      }

  def emitFuncHeader(func: FuncDef, entry: Boolean = false): Doc = {
    text(s"#pragma HLS INLINE") <@>
    (if (entry)
      vsep(func.args.map(arg =>
        arg.typ match {
          case _:TArray => text(s"#pragma HLS INTERFACE s_axilite port=${arg.id}")
          case _ => emptyDoc
        }
      ))
     else emptyDoc) <@>
    vsep(bankPragmas(func.args))
  }

  def emitArrayDecl(ta: TArray, id: Id): Doc =
    emitType(ta.typ) <+> id <> generateDims(ta.dims)

  def generateDims(dims: List[(Int, Int)]): Doc =
    ssep(dims.map(d => brackets(value(d._1))), emptyDoc)

  def emitType(typ: Type): Doc = typ match {
    case _:TVoid => text("void")
    case _:TBool | _:TIndex | _:TStaticInt => text("int")
    case _:TFloat => text("float")
    case _:TDouble => text("double")
    case TSizedInt(s, un) => text(if (un) s"ap_uint<$s>" else s"ap_int<$s>")
    case TArray(typ, _) => emitType(typ)
    case TRecType(n, _) => text(n.toString)
    case _:TFun => throw Impossible("Cannot emit function types")
    case TAlias(n) => text(n.toString)
  }

  def emitProg(p: Prog, c: Config): String = {
    val layout =
      CppPreamble <@>
      vsep(p.includes.map(emitInclude)) <@>
      vsep(p.defs.map(emitDef)) <@>
      vsep(p.decors.map(d => text(d.value))) <@>
      emitFunc(FuncDef(Id(c.kernelName), p.decls, TVoid(), Some(p.cmd)), true)

    layout.pretty
  }

}

private class VivadoBackendHeader extends VivadoBackend {
  override def emitCmd(c: Command): Doc = emptyDoc

  override def emitFunc(func: FuncDef, entry: Boolean): Doc = func match { case FuncDef(id, args, ret, _) =>
    val as = hsep(args.map(d => emitDecl(d.id, d.typ)), comma)
    emitType(ret) <+> id <> parens(as) <> semi
  }

  override def emitProg(p: Prog, c: Config) = {
    val declarations =
      vsep(p.includes.map(emitInclude) ++ p.defs.map(emitDef)) <@>
      emitFunc(FuncDef(Id(c.kernelName), p.decls, TVoid(), None))

    declarations.pretty
  }
}

case object VivadoBackend extends Backend {
  def emitProg(p: Prog, c: Config) = c.header match {
    case true => (new VivadoBackendHeader()).emitProg(p, c)
    case false => (new VivadoBackend()).emitProg(p, c)
  }
  val canGenerateHeader = true
}
