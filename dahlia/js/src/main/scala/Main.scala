package fuselang

import scala.scalajs.js
import js.annotation._

import scala.util.Try

import common.Errors

@JSExportTopLevel("Dahlia")
object Dahlia {
  def formatErrorMsg(err: Errors.DahliaError) = {
    val (msg, ctx, postMsg) = (err.getMsg, err.getCtx, err.getPostMsg)
    s"[$ctx] $msg\n$postMsg"
  }
  /**
   * Return a string representing the compiled program or return a line position.
   */
  @JSExport
  def compileString(prog: String): js.Tuple2[String, js.Tuple2[Int, String]] = {
    Try(Compiler.codegen(Compiler.checkStringWithError(prog)))
      .toEither.left.map(err => err match {
        case e: Errors.DahliaError => {
          val errDetails: js.Tuple2[Int, String] =
            (Option(e.getPos).map(loc => loc.line).getOrElse(-1), formatErrorMsg(e))
          ("", errDetails)
        }
        case _ => {
          val errDetails: js.Tuple2[Int, String] = (-1, err.getMessage)
          ("", errDetails)
        }
      })
      .map(out => {
        val errDetails: js.Tuple2[Int, String] = (-1, "")
        (out, errDetails)
      })
      .merge
  }
}
