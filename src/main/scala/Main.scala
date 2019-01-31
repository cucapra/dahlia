package fuselang

object Main {
  import Syntax._
  import FuseLangDSL._
  import TypeChecker.checkFuse
  import CCodeGen.pretty

  def main(args: Array[String]) = {
    val prog =
      Fuse {List(
        "sum" := 10,
        For ("i", 0 upto 10 unroll 5) {
          EBinop(OpAdd, "sum", 10)
        }
      )}

    val env = checkFuse(prog)
    println(pretty(prog)(env))
  }
}
