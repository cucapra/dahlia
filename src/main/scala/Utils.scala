package fuselang

object Utils {
  import FuseParser._
  import TypeChecker._
  import Emit._

  def compile(s: String) = {
    val ast = parse(s)
    val env = checkFuse(ast)
    emitC(ast, env)
  }

}
