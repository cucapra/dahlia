package fuselang

object Main {
  import FuseParser.parse
  //import TypeChecker.checkFuse
  //import Emit.emitC

  def main(args: Array[String]) = {
    val prog = """
    let x = 1;
    x := 1;
    if (x == 1) {
      x := 2;
    };
    for (let i = 0..10) unroll 3 {
    }
    """
    println(parse(prog))
  }
}
