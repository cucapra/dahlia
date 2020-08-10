package fuselang.passes

import scala.{PartialFunction => PF}
import fuselang.common._
import Transformer._
import EnvHelpers._
import Syntax._

object Sequentialize extends PartialTransformer {

  type Env = UnitEnv
  val emptyEnv = UnitEnv()


  def myRewriteC: PF[(Command, Env), (Command, Env)] = {
    case (CPar(cmds), env) => CSeq(cmds.map(c => rewriteC(c)(env)._1)) -> env
  }

  override def rewriteC(cmd: Command)(implicit env: Env) =
    mergeRewriteC(myRewriteC)(cmd, env)
  // No need to traverse expressions
  override def rewriteE(expr: Expr)(implicit env: Env) = (expr, env)
}
