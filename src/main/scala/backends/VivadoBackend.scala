package fuselang.backend

import Cpp._

import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._
import PrettyPrint.Doc
import PrettyPrint.Doc._

private class VivadoBackend extends CppLike {
  val CppPreamble: Doc = text("""
    |#include <ap_int.h>
  """.stripMargin.trim)

  def unroll(n: Int): Doc = n match {
    case 1 => emptyDoc
    case n => value(s"#pragma HLS UNROLL factor=$n skip_exit_check") <@> line
  }

  def bank(id: Id, banks: List[Int]): List[Doc] = banks.zipWithIndex.map({
    case (bank, dim) =>
      if (bank != 1) {
        text(
          s"#pragma HLS ARRAY_PARTITION variable=$id cyclic factor=$bank dim=${dim + 1}")
      } else {
        emptyDoc
      }
  })

  def bankPragmas(decls: List[Decl]): List[Doc] = decls
    .collect({ case Decl(id, typ: TArray) => vsep(bank(id, typ.dims.map(_._2))) })

  def bankWarn(decls: List[Decl]) =
    decls.collect({ case Decl(id, typ: TArray) =>
      typ.dims.foreach({ case (_, bank) =>
        if (bank > 1)
          throw BackendError(
            s"Interfact array `${id}' is partitioned. SDAccel will generate incorrect hardware for partitioned interface arrays.")
      })
    })

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
    val argPragmas = func.args.map(arg =>
      arg.typ match {
        case _:TArray => {
          text(s"#pragma HLS INTERFACE m_axi port=${arg.id} offset=slave bundle=gmem") <@>
            text(s"#pragma HLS INTERFACE s_axilite port=${arg.id} bundle=control")
        }
        case _ =>
          text(s"#pragma HLS INTERFACE s_axilite port=${arg.id} bundle=control")
      })

    if (entry) bankWarn(func.args)

    (if (entry) vsep(argPragmas) else text(s"#pragma HLS INLINE")) <@>
      vsep(bankPragmas(func.args)) <@>
      text(s"#pragma HLS INTERFACE s_axilite port=return bundle=control") <@>
      emptyDoc
  }

  def emitArrayDecl(ta: TArray, id: Id): Doc =
    emitType(ta.typ) <+> id <> generateDims(ta.dims)

  def generateDims(dims: List[DimSpec]): Doc =
    ssep(dims.map(d => brackets(value(d._1))), emptyDoc)

  def emitType(typ: Type): Doc = typ match {
    case _:TVoid => text("void")
    case _:TBool | _:TIndex => text("int")
    case _:TStaticInt => throw Impossible("TStaticInt type should not exist")
    case _:TFloat => text("float")
    case _:TDouble => text("double")
    case _:TRational => throw Impossible("Rational type should not exist")
    case TSizedInt(s, un) => text(if (un) s"ap_uint<$s>" else s"ap_int<$s>")
    case TFixed(t,i,un) => text(if (un) s"ap_ufixed<$t,$i>" else s"ap_fixed<$t,$i>")
    case TArray(typ, _, _) => emitType(typ)
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

  override def emitFunc(func: FuncDef, entry: Boolean): Doc = func match {
    case FuncDef(id, args, ret, _) =>
      val as = commaSep(args.map(d => emitDecl(d.id, d.typ)))
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
