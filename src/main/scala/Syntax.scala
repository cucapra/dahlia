package fuselang

import scala.util.parsing.input.Positional

object Syntax {

  import Errors._
  import scala.math.{max,log10,ceil}

  case class Id(v: String) extends Positional {
    override def toString = s"`$v'"
  }

  sealed trait Type extends Positional {
    def :<(that: Type): Boolean = (this, that) match {
      case (TStaticInt(_), TStaticInt(_)) => true
      case (TStaticInt(_), TSizedInt(_)) | (TSizedInt(_), TStaticInt(_)) => true
      case (TSizedInt(v1), TSizedInt(v2)) => v1 <= v2
      case _ => this == that
    }

    def join(that: Type, op: (Int, Int) => Int): Type = (this, that) match {
      case (TSizedInt(s1), TSizedInt(s2)) => TSizedInt(max(s1, s2))
      case (TStaticInt(v1), TStaticInt(v2)) => TStaticInt(op(v1, v2))
      case (TStaticInt(v), TSizedInt(s)) => {
        TSizedInt(max(s, ceil(log10(v)/log10(2)).toInt))
      }
      case (TSizedInt(s), TStaticInt(v)) => {
        TSizedInt(max(s, ceil(log10(v)/log10(2)).toInt))
      }
      case (t1, t2) => throw NoJoin(t1, t2)
    }

    override def toString = this match {
      case TBool => "bool"
      case TFloat => "float"
      case TSizedInt(l) => s"int$l"
      case TStaticInt(s) => s"static($s)"
      case TArray(t, dims) =>
        s"$t" + dims.foldLeft("")({ case (acc, (d, b)) => s"$acc[$d bank $b]" })
      case TIndex(s, d) => s"idx($s, $d)"
    }
  }
  case object TBool extends Type
  case class TSizedInt(len: Int) extends Type
  case class TStaticInt(v: Int) extends Type
  case object TFloat extends Type
  case class TArray(typ: Type, dims: List[(Int, Int)]) extends Type
  case class TIndex(static: (Int, Int), dynamic: (Int, Int)) extends Type

  sealed trait Op2 extends Positional {
    override def toString = this match {
      case OpEq => "=="
      case OpNeq => "!="
      case OpLt => "<"
      case OpLte => "<="
      case OpGt => ">"
      case OpGte => ">="
      case OpAdd => "+"
      case OpSub => "-"
      case OpTimes => "*"
      case OpDiv => "/"
    }

    def toFun: (Int, Int) => Int = this match {
      case OpAdd => _ + _
      case OpTimes => _ * _
      case OpDiv => _ / _
      case OpSub => _ - _
      case _ => throw MsgError(s"toFun not defined on $this")
    }
  }
  case object OpEq extends Op2
  case object OpNeq extends Op2
  case object OpAdd extends Op2
  case object OpSub extends Op2
  case object OpTimes extends Op2
  case object OpDiv extends Op2
  case object OpLt extends Op2
  case object OpLte extends Op2
  case object OpGt extends Op2
  case object OpGte extends Op2

  sealed trait Expr extends Positional
  case class EInt(v: Int) extends Expr
  case class EFloat(f: Float) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: Op2, e1: Expr, e2: Expr) extends Expr
  case class EAA(id: Id, idxs: List[Expr]) extends Expr
  case class EVar(id: Id) extends Expr

  case class CRange(s: Int, e: Int, u: Int) extends Positional {
    def idxType: TIndex = {
      if ((e - s) % u != 0) {
        throw UnrollRangeError(e - s, u)
      } else {
        TIndex((0, u), (s/u, e/u))
      }
    }
  }

  case class CReducer(seq: Command) extends Positional

  sealed trait Command extends Positional
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CLet(id: Id, e: Expr) extends Command
  case class CIf(cond: Expr, cons: Command) extends Command
  case class CFor(iter: Id, range: CRange, par: Command, seq: CReducer) extends Command
  case class CUpdate(lhs: Expr, rhs: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case class CDecl(id: Id, typ: Type) extends Command
  case object CRefreshBanks extends Command
  case object CEmpty extends Command
}
