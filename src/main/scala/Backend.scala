package fuselang

/**
 * Abstract definition of a Fuse backend.
 */
trait Backend {

  def emitProg(p: Syntax.Prog, c: Utils.Config): String

}
