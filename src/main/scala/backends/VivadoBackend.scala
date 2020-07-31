package fuselang.backend

import Cpp._

import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._
import PrettyPrint.Doc
import PrettyPrint.Doc._

private class VivadoBackend(config: Config) extends CppLike {
  val CppPreamble: Doc = text("""
    |#include <ap_int.h>
  """.stripMargin.trim)

  def unroll(n: Int): Doc =
    value(s"#pragma HLS UNROLL factor=$n skip_exit_check")

  def interfaceValid(decls: List[Decl]) =
    decls.collect({
      case Decl(id, typ: TArray) => {
        if (typ.ports > 1)
          throw BackendError(
            s"Multiported array argument `${id}' is disallowed. SDAccel inconsistently fails with RESOURCE pragma on argument arrays."
          )
        typ.dims.foreach({
          case (_, bank) =>
            if (bank > 1)
              throw BackendError(
                s"Partitioned array argument `${id}' is disallowed. SDAccel generates incorrect hardware for partitioned argument arrays."
              )
        })
      }
    })

  def bankAndResource(id: Id, ports: Int, banks: List[Int]): Doc = {
    val bankPragma = banks.zipWithIndex.map({
      case (1, _) => emptyDoc
      case (bank, dim) =>
        text(
          s"#pragma HLS ARRAY_PARTITION variable=$id cyclic factor=$bank dim=${dim + 1}"
        )
    })
    val resource = ports match {
      case 1 => "RAM_1P_BRAM"
      case 2 => "RAM_T2P_BRAM"
      case n =>
        throw BackendError(s"SDAccel does not support ${n}-ported memories.")
    }
    val resPragma = text(
      s"#pragma HLS resource variable=${id} core=${resource}"
    )
    vsep(resPragma :: bankPragma)
  }

  def memoryPragmas(decls: List[Decl]): List[Doc] =
    decls
      .collect({
        case Decl(id, typ: TArray) =>
          bankAndResource(id, typ.ports, typ.dims.map(_._2))
      })

  override def emitLet(let: CLet): Doc = {
    super.emitLet(let) <@>
      (let.typ match {
        case Some(t) => vsep(memoryPragmas(List(Decl(let.id, t))))
        case None => emptyDoc
      })
  }

  def emitPipeline(enabled: Boolean): Doc =
    if (enabled) value(s"#pragma HLS PIPELINE") <> line else emptyDoc

  def emitFor(cmd: CFor): Doc =
    text("for") <> emitRange(cmd.range) <+> scope {
      emitPipeline(cmd.pipeline) <>
        unroll(cmd.range.u) <@>
        text("#pragma HLS LOOP_FLATTEN off") <@>
        cmd.par <>
        (if (cmd.combine != CEmpty) line <> text("// combiner:") <@> cmd.combine
         else emptyDoc)
    }

  override def emitWhile(cmd: CWhile): Doc =
    text("while") <> parens(cmd.cond) <+> scope {
      emitPipeline(cmd.pipeline) <>
        text("#pragma HLS LOOP_FLATTEN off") <@>
        cmd.body
    }

  private def axiHeader(arg: Syntax.Decl): Doc = {
    arg.typ match {
      case _: TArray => {
        text(
          s"#pragma HLS INTERFACE m_axi port=${arg.id} offset=slave bundle=gmem"
        ) <@>
          text(
            s"#pragma HLS INTERFACE s_axilite port=${arg.id} bundle=control"
          )
      }
      case _ =>
        text(
          s"#pragma HLS INTERFACE s_axilite port=${arg.id} bundle=control"
        )
    }
  }

  private def apMemoryHeader(arg: Syntax.Decl): Doc = {
    arg.typ match {
      case _: TArray => {
        text(s"#pragma HLS INTERFACE ap_memory port=${arg.id}")
      }
      case _ =>
        text(
          s"#pragma HLS INTERFACE s_axilite port=${arg.id} bundle=control"
        )
    }
  }

  def emitFuncHeader(func: FuncDef, entry: Boolean = false): Doc = {
    // Error if function arguments are partitioned/ported.
    interfaceValid(func.args)

    if (entry) {
      val argPragmas = func.args.map(arg =>
        config.memoryInterface match {
          case Axi => axiHeader(arg)
          case ApMemory => apMemoryHeader(arg)
        }
      )
      vsep(argPragmas) <@>
        text(s"#pragma HLS INTERFACE s_axilite port=return bundle=control")
    } else {
      text(s"#pragma HLS INLINE")
    }

  }

  def emitArrayDecl(ta: TArray, id: Id): Doc =
    emitType(ta.typ) <+> id <> generateDims(ta.dims)

  def generateDims(dims: List[DimSpec]): Doc =
    ssep(dims.map(d => brackets(value(d._1))), emptyDoc)

  def emitType(typ: Type): Doc = typ match {
    case _: TVoid => text("void")
    case _: TBool | _: TIndex => text("int")
    case _: TStaticInt => throw Impossible("TStaticInt type should not exist")
    case _: TFloat => text("float")
    case _: TDouble => text("double")
    case _: TRational => throw Impossible("Rational type should not exist")
    case TSizedInt(s, un) => text(if (un) s"ap_uint<$s>" else s"ap_int<$s>")
    case TFixed(t, i, un) =>
      text(if (un) s"ap_ufixed<$t,$i>" else s"ap_fixed<$t,$i>")
    case TArray(typ, _, _) => emitType(typ)
    case TRecType(n, _) => text(n.toString)
    case _: TFun => throw Impossible("Cannot emit function types")
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

private class VivadoBackendHeader(c: Config) extends VivadoBackend(c) {
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
    case true => (new VivadoBackendHeader(c)).emitProg(p, c)
    case false => (new VivadoBackend(c)).emitProg(p, c)
  }
  val canGenerateHeader = true
}
