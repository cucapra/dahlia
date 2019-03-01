package fuselang.backend

import fuselang.Syntax._
import fuselang.Errors._
import fuselang.Utils._

import Cpp._

private class VivadoBackend extends CppLike {

  def typeToDoc(typ: Type) = typ match {
    case _:TVoid => "void"
    case _:TBool | _:TIndex | _:TStaticInt => "int"
    case _:TFloat => "float"
    case TSizedInt(s) => s"ap_int<$s>"
    case TArray(typ, _) => typ.toString
    case TRecType(n, _) => n
    case _:TFun => throw Impossible("Cannot emit function types")
    case TAlias(n) => n
  }
}

case object VivadoBackend extends Backend {
  private val emitter = new VivadoBackend()
  def emitProg(p: Prog, c: Config) = emitter.emitProg(p, c)
}
