package fuselang

//import scala.scalajs.js
import scala.scalajs.js.annotation._

//@ScalaJSDefined
//@JSExportTopLevel("Babar")
//class Foobaz(x: String) extends js.Object {
  //val inner = new JSFoo(x.length)

  //def method(y: String): Int = x + y
//}

@JSExportTopLevel("Dahlia")
object Dahlia {
  @JSExport
  def add(x: Int, y: Int) = x + y
  @JSExport
  def compileString = Compiler.checkStringWithError _
}
