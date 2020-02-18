package fuselang

import common._
import Configuration._
import Syntax._

object Compiler {

  def showDebug(ast: Prog, pass: String, c: Config): Unit = {
    if (c.passDebug) {
      val top = ("=" * 15) + pass + ("=" * 15)
      println(top)
      println(Pretty.emitProg(ast)(true).trim)
      println("=" * top.length)
    }
  }

  def toBackend(str: BackendOption): fuselang.backend.Backend = str match {
    case Vivado => backend.VivadoBackend
    case Cpp    => backend.CppRunnable
    case Futil  => backend.futil.FutilBackend
  }

  def checkStringWithError(prog: String, c: Config = emptyConf) = {
    val ast = FuseParser.parse(prog)
    passes.WellFormedChecker.check(ast)
    typechecker.TypeChecker.typeCheck(ast); showDebug(ast, "Type Checking", c)
    passes.BoundsChecker.check(ast);      // Doesn't modify the AST.
    passes.LoopChecker.check(ast);        // Doesn't modify the AST.
    passes.DependentLoops.check(ast);     // Doesn't modify the AST.
    typechecker.CapabilityChecker.check(ast); showDebug(ast, "Capability Checking", c)
    typechecker.AffineChecker.check(ast); // Doesn't modify the AST
    ast
  }

  def codegen(ast: Prog, c: Config = emptyConf) = {
    val rast = passes.RewriteView.rewriteProg(ast); showDebug(rast, "Rewrite Views", c)
    toBackend(c.backend).emit(rast, c)
  }

}
