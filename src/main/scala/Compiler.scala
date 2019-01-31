package fuselang

object Compiler {

  def compileString(prog: String) = {
    val ast = FuseParser.parse(prog)
    val env = TypeChecker.checkFuse(ast)
    Emit.emitC(ast, env)
  }
}
