package fuselang.backend

/**
 * Abstract definition of a Fuse backend.
 */
trait Backend {

  def emitProg(p: fuselang.Syntax.Prog, c: fuselang.Utils.Config): String

}
