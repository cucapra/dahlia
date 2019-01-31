package fuselang

object Utils {
  import FuseParser._
  import TypeChecker._
  //import Emit._

  def parseAst(s: String) = {
    parse(s)
  }

  def typeCheck(s: String) = {
    val ast = parse(s)
    checkFuse(ast)
  }

}
