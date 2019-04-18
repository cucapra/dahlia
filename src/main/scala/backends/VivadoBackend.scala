package fuselang.backend

import fuselang.Syntax._
import fuselang.Errors._
import fuselang.Utils._

import Cpp._

private class VivadoBackend extends CppLike {

  def unroll(n: Int) = n match {
    case 1 => ""
    case n => s"#pragma HLS UNROLL factor=$n skip_exit_check"
  }

  def bank(id: Id, n: List[Int]): String = n.foldLeft(1)(_ * _) match {
    case 1 => ""
    case n => s"#pragma HLS ARRAY_PARTITION variable=$id factor=$n"
  }

  def bankPragmas(decls: List[Decl]) = decls
    .collect({ case Decl(id, typ: TArray) => bank(id, typ.dims.map(_._2)) })
    .withFilter(s => s != "")
    .map(s => value(s))

  def emitFor(cmd: CFor): Doc =
    "for" <> emitRange(cmd.range) <+> scope {
      unroll(cmd.range.u) <@>
      cmd.par <@>
      (if (cmd.combine != CEmpty) text("// combiner:") <@> cmd.combine
       else value(""))
    }

  def emitFuncHeader(func: FuncDef): Doc = {
    vsep(bankPragmas(func.args))
  }

  def emitArrayDecl(ta: TArray, id: Id) =
    emitType(ta.typ) <+> id <> generateDims(ta.dims)

  def generateDims(dims: List[(Int, Int)]): Doc =
    brackets(value(dims.foldLeft(1)({ case (acc, (l, _)) => acc * l})))

  def emitType(typ: Type) = typ match {
    case _:TVoid => "void"
    case _:TBool | _:TIndex | _:TStaticInt => "int"
    case _:TFloat => "float"
    case TSizedInt(s) => s"ap_int<$s>"
    case TArray(typ, _) => typ.toString
    case TRecType(n, _) => n
    case _:TFun => throw Impossible("Cannot emit function types")
    case TAlias(n) => n
  }

  def emitProg(p: Prog, c: fuselang.Utils.Config): String = {
    val layout =
      vsep(p.includes.map(emitInclude)) <@>
      vsep(p.defs.map(emitDef)) <@>
      emitFunc(FuncDef(Id(c.kernelName), p.decls, Some(p.cmd)))

    super.pretty(layout).layout
  }

}

case object VivadoBackend extends Backend {
  private val emitter = new VivadoBackend()
  def emitProg(p: Prog, c: Config) = emitter.emitProg(p, c)
}
