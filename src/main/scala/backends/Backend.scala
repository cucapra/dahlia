package fuselang.backend

import fuselang.CompilerError.BackendError

/**
 * Abstract definition of a Fuse backend.
 */
trait Backend {

  def emit(p: fuselang.Syntax.Prog, c: fuselang.Utils.Config): String = {
    if (c.header && (canGenerateHeader == false)) {
      throw BackendError(s"Backend $this does not support header generation.")
    }
    emitProg(p, c)
  }

  /**
   * Generate a String representation of the Abstract Syntax Tree of the
   * program. Assumes that typechecking pass has been done on the AST and
   * might use the type decorations added to the nodes.
   */
  def emitProg(p: fuselang.Syntax.Prog, c: fuselang.Utils.Config): String

  /**
   * True if the backend can generate header files.
   */
  val canGenerateHeader: Boolean

}
