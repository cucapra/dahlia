package fuselang

import scala.scalajs.js.annotation._

@JSExportTopLevel("Dahlia")
object Dahlia {
  @JSExport
  def compileString(prog: String) =
    Compiler.codegen(Compiler.checkStringWithError(prog))
}
