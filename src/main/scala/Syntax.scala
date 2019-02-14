package fuselang

import scala.util.parsing.input.Positional

object Syntax {

  import Errors._
  import scala.math.{max,log10,ceil}

  case class Id(v: String) extends Positional {
    var typ: Option[Type] = None;
    override def toString = s"$v"
  }

  sealed trait Type extends Positional {
    // XXX(rachit): @adrian these are the Subtyping functions
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
      case _: TVoid => "void"
      case _: TBool => "bool"
      case _: TFloat => "float"
      case TSizedInt(l) => s"int$l"
      case TStaticInt(s) => s"static($s)"
      case TArray(t, dims) =>
        s"$t" + dims.foldLeft("")({ case (acc, (d, b)) => s"$acc[$d bank $b]" })
      case TIndex(s, d) => s"idx($s, $d)"
      case TFun(args) => args.foldLeft("")({ case (acc, t) => s"$acc$t -> "}) + "void"
    }
  }
  // Use case class instead of case object to get unique positions
  case class TVoid() extends Type
  case class TBool() extends Type
  case class TFloat() extends Type
  case class TSizedInt(len: Int) extends Type
  case class TStaticInt(v: Int) extends Type
  case class TFun(args: List[Type]) extends Type
  case class TArray(typ: Type, dims: List[(Int, Int)]) extends Type
  case class TIndex(static: (Int, Int), dynamic: (Int, Int)) extends Type

  sealed trait BOp extends Positional {
    override def toString = this match {
      case _:OpEq => "=="
      case _:OpNeq => "!="
      case _:OpLt => "<"
      case _:OpLte => "<="
      case _:OpGt => ">"
      case _:OpGte => ">="
      case _:OpAnd => "&&"
      case _:OpOr => "||"
      case _:OpAdd => "+"
      case _:OpSub => "-"
      case _:OpMul => "*"
      case _:OpDiv => "/"
      case _:OpMod => "%"
      case _:OpLsh => "<<"
      case _:OpRsh => ">>"
      case _:OpBAnd => "&"
      case _:OpBOr => "|"
      case _:OpBXor => "^"
    }

    def toFun: (Int, Int) => Int = this match {
      case _:OpAdd => _ + _
      case _:OpMul => _ * _
      case _:OpDiv => _ / _
      case _:OpSub => _ - _
      case _:OpBAnd => _ & _
      case _:OpBOr => _ | _
      case _:OpBXor => _ ^ _
      case _ => throw MsgError(s"toFun not defined on $this")
    }
  }
  // comparison ops
  case class OpEq() extends BOp
  case class OpNeq() extends BOp
  case class OpLt() extends BOp
  case class OpLte() extends BOp
  case class OpGt() extends BOp
  case class OpGte() extends BOp
  // Boolean ops
  case class OpAnd() extends BOp
  case class OpOr() extends BOp
  // integer/float ops
  case class OpAdd() extends BOp
  case class OpSub() extends BOp
  case class OpMul() extends BOp
  case class OpDiv() extends BOp
  case class OpMod() extends BOp
  // Shifting ops
  case class OpLsh() extends BOp
  case class OpRsh() extends BOp
  // Bit ops
  case class OpBAnd() extends BOp
  case class OpBOr() extends BOp
  case class OpBXor() extends BOp

  sealed trait Expr extends Positional {
    def isLVal = this match {
      case _:EVar | _:EAA => true
      case _ => false
    }
  }
  case class EInt(v: Int) extends Expr
  case class EFloat(f: Float) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr
  case class EAA(id: Id, idxs: List[Expr]) extends Expr
  case class EApp(func: Id, args: List[Expr]) extends Expr
  case class EVar(id: Id) extends Expr

  case class CRange(iter: Id, s: Int, e: Int, u: Int) extends Positional {
    def idxType: TIndex = {
      if ((e - s) % u != 0) {
        throw UnrollRangeError(this.pos, e - s, u)
      } else {
        TIndex((0, u), (s/u, e/u))
      }
    }

  }

  sealed trait ROp extends Positional {
    override def toString = this match {
      case _: RAdd => "+="
      case _: RMul => "*="
      case _: RSub => "-="
      case _: RDiv => "/="
    }
  }
  case class RAdd() extends ROp
  case class RMul() extends ROp
  case class RSub() extends ROp
  case class RDiv() extends ROp

  sealed trait ViewType extends Positional
  case class Shrink(arrId: Id, dims: List[(Expr,Int,Int)]) extends ViewType {
    dims.forall({ case (_, w, s) => {
      if (w != s) {
        throw MalformedShrink(this, w, s)
      }
      true
    }})
  }

  sealed trait Command extends Positional
  case class CPar(c1: Command, c2: Command) extends Command
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CLet(id: Id, var typ: Option[Type], e: Expr) extends Command
  case class CView(id: Id, kind: ViewType) extends Command
  case class CIf(cond: Expr, cons: Command) extends Command
  case class CFor(range: CRange, par: Command, combine: Command) extends Command
  case class CUpdate(lhs: Expr, rhs: Expr) extends Command {
    if (lhs.isLVal == false) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CReduce(rop: ROp, lhs: Expr, rhs: Expr) extends Command {
    if (lhs.isLVal == false) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CExpr(exp: Expr) extends Command
  case object CEmpty extends Command

  case class Decl(id: Id, typ: Type) extends Positional
  case class FDef(id: Id, args: List[Decl], body: Command) extends Positional

  case class Prog(fdefs: List[FDef], decls: List[Decl], cmd: Command) extends Positional
}
