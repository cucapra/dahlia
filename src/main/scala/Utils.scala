package fuselang

object Utils {

  def parseAst(s: String) = FuseParser.parse(s)
  def typeCheck(s: String) = TypeChecker.checkFuse(FuseParser.parse(s))

}
