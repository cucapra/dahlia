package fuselang.common

import scala.util.parsing.input.{Positional, Position}
import scala.math.abs

import Errors._
import Configuration.BackendOption

object Syntax {

  /**
    * Annotations added by the various passes of the type checker.
    */
  object Annotations {
    sealed trait Consumable
    case object ShouldConsume extends Consumable
    case object SkipConsume extends Consumable

    sealed trait ConsumableAnnotation {
      var consumable: Option[Consumable] = None
    }

    sealed trait TypeAnnotation {
      var typ: Option[Type] = None;
    }
  }

  object OpConstructor {
    val add: (Double, Double) => Double = (_ + _)
    val mul: (Double, Double) => Double = (_ * _)
    val div: (Double, Double) => Double = (_ / _)
    val sub: (Double, Double) => Double = (_ - _)
    val mod: (Double, Double) => Double = (_ % _)
  }

  import Annotations._

  case class Id(v: String) extends Positional with TypeAnnotation {
    override def toString: String = s"$v"
  }

  // Capabilities for read/write
  sealed trait Capability
  case object Read extends Capability
  case object Write extends Capability

  sealed trait Type extends Positional {
    override def toString: String = this match {
      case _: TVoid => "void"
      case _: TBool => "bool"
      case _: TRational => "rational"
      case _: TFloat => "float"
      case _: TDouble => "double"
      case TFixed(t, i, un) => s"${if (un) "u" else ""}fix<$t,$i>"
      case TSizedInt(l, un) => s"${if (un) "u" else ""}bit<$l>"
      case TStaticInt(s) => s"static($s)"
      case TArray(t, dims, p) =>
        (if (p > 1) s"$t{$p}" else s"$t") + dims.foldLeft("")({
          case (acc, (d, b)) => s"$acc[$d${if (b > 1) s" bank $b" else ""}]"
        })
      case TIndex(s, d) => s"idx($s, $d)"
      case TFun(args, ret) => s"${args.mkString("->")} -> ${ret}"
      case TRecType(n, _) => s"$n"
      case TAlias(n) => n.toString
    }
  }
  // Types that can be upcast to Ints
  sealed trait IntType
  case class TSizedInt(len: Int, unsigned: Boolean) extends Type with IntType
  case class TStaticInt(v: Int) extends Type with IntType
  case class TIndex(static: (Int, Int), dynamic: (Int, Int))
      extends Type
      with IntType {
    // Our ranges are represented as s..e with e excluded from the range.
    // Therefore, the maximum value is one than the product of the interval ends.
    val maxVal: Int = static._2 * dynamic._2 - 1
  }
  // Use case class instead of case object to get unique positions
  case class TVoid() extends Type
  case class TBool() extends Type

  case class TRational(value: String) extends Type
  case class TFloat() extends Type
  case class TDouble() extends Type
  case class TFixed(ltotal: Int, lint: Int, unsigned: Boolean) extends Type
  case class TFun(args: Seq[Type], ret: Type) extends Type
  case class TRecType(name: Id, fields: Map[Id, Type]) extends Type
  case class TAlias(name: Id) extends Type

  // Each dimension has a length and a bank
  type DimSpec = (Int, Int)
  case class TArray(typ: Type, dims: Seq[DimSpec], ports: Int) extends Type {
    dims.zipWithIndex.foreach({
      case ((len, bank), dim) =>
        if (bank > len || len % bank != 0) {
          throw MalformedType(
            s"Dimension $dim of TArray is malformed. Full type: $this"
          )
        }
    })
  }

  sealed trait BOp extends Positional {
    val op: String;
    override def toString: String = this.op
    def toFun: Option[(Double, Double) => Double] = this match {
      case n: NumOp => Some(n.fun)
      case _ => None
    }
  }

  case class EqOp(op: String) extends BOp
  case class CmpOp(op: String) extends BOp
  case class BoolOp(op: String) extends BOp
  case class NumOp(op: String, fun: (Double, Double) => Double) extends BOp
  case class BitOp(op: String) extends BOp

  sealed trait Expr extends Positional with TypeAnnotation {
    def isLVal: Boolean = this match {
      case _: EVar | _: EArrAccess | _: EPhysAccess => true
      case _ => false
    }
  }
  case class EInt(v: Int, base: Int = 10) extends Expr
  case class ERational(d: String) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr
  case class EArrAccess(id: Id, idxs: Seq[Expr])
      extends Expr
      with ConsumableAnnotation
  case class EPhysAccess(id: Id, bankIdxs: Seq[(Int, Expr)])
      extends Expr
      with ConsumableAnnotation
  case class EArrLiteral(idxs: Seq[Expr]) extends Expr
  case class ERecAccess(rec: Expr, fieldName: Id) extends Expr
  case class ERecLiteral(fields: Map[Id, Expr]) extends Expr
  case class EApp(func: Id, args: Seq[Expr]) extends Expr
  case class EVar(id: Id) extends Expr
  case class ECast(e: Expr, castType: Type) extends Expr

  case class CRange(
      iter: Id,
      castType: Option[Type],
      reversed: Boolean,
      s: Int,
      e: Int,
      u: Int
  ) extends Positional {
    def idxType: TIndex = {
      if (abs(e - s) % u != 0) {
        throw UnrollRangeError(this.pos, e - s, u)
      } else {
        TIndex((0, u), (s / u, e / u))
      }
    }
  }

  case class ROp(op: String) extends Positional {
    override def toString: String = this.op
  }

  /** Views **/
  sealed trait Suffix extends Positional
  case class Aligned(factor: Int, e: Expr) extends Suffix
  case class Rotation(e: Expr) extends Suffix

  /**
    * Represents the view configuration over one dimension. Prefixes can only
    * be specified as +n where `n` is a number added to the suffix.
    *
    * If the suffix is missing, it is assumed to be 0.
    * If the shrink is None, then the original banking factor of the array is
    * retained.
    */
  case class View(suffix: Suffix, prefix: Option[Int], shrink: Option[Int])
      extends Positional

  sealed trait Command extends Positional {
    var attributes: Map[String, Int] = Map()
  }
  case class CPar(cmds: Seq[Command]) extends Command
  case class CSeq(cmds: Seq[Command]) extends Command
  case class CLet(id: Id, var typ: Option[Type], e: Option[Expr])
      extends Command
  case class CView(id: Id, arrId: Id, dims: Seq[View]) extends Command
  case class CSplit(id: Id, arrId: Id, factors: Seq[Int]) extends Command
  case class CIf(cond: Expr, cons: Command, alt: Command) extends Command
  case class CFor(
      range: CRange,
      pipeline: Boolean,
      par: Command,
      combine: Command
  ) extends Command
  case class CWhile(cond: Expr, pipeline: Boolean, body: Command)
      extends Command
  case class CDecorate(value: String) extends Command
  case class CUpdate(lhs: Expr, rhs: Expr) extends Command {
    if (lhs.isLVal == false) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CReduce(rop: ROp, lhs: Expr, rhs: Expr) extends Command {
    if (lhs.isLVal == false) throw UnexpectedLVal(lhs, "reduction")
  }
  case class CReturn(exp: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case class CBlock(cmd: Command) extends Command
  case object CEmpty extends Command

  // Smart constructors for composition
  object CPar {
    def smart(c1: Command, c2: Command): Command = (c1, c2) match {
      case (l: CPar, r: CPar) => l.copy(cmds = l.cmds ++ r.cmds)
      case (l: CPar, r) => l.copy(cmds = l.cmds :+ r)
      case (l, r: CPar) => r.copy(cmds = l +: r.cmds)
      case (CEmpty, r) => r
      case (l, CEmpty) => l
      case _ => CPar(Seq(c1, c2))
    }

    def smart(cmds: Seq[Command]): Command = {
      val flat = cmds.flatMap(cmd =>
        cmd match {
          case CPar(cs) => cs
          case CEmpty => Seq()
          case _ => Seq(cmd)
        }
      )
      if (flat.length == 0) {
        CEmpty
      } else if (flat.length == 1) {
        flat(0)
      } else {
        CPar(flat)
      }
    }
  }
  object CSeq {
    def smart(c1: Command, c2: Command): Command = (c1, c2) match {
      case (l: CSeq, r: CSeq) => l.copy(cmds = l.cmds ++ r.cmds)
      case (l: CSeq, r) => l.copy(cmds = l.cmds :+ r)
      case (l, r: CSeq) => r.copy(cmds = l +: r.cmds)
      case (CEmpty, r) => r
      case (l, CEmpty) => l
      case _ => CSeq(Seq(c1, c2))
    }

    def smart(cmds: Seq[Command]): Command = {
      val flat = cmds.flatMap(cmd =>
        cmd match {
          case CSeq(cs) => cs
          case CEmpty => Seq()
          case _ => Seq(cmd)
        }
      )
      if (flat.length == 0) {
        CEmpty
      } else if (flat.length == 1) {
        flat(0)
      } else {
        CSeq(flat)
      }
    }
  }

  sealed trait Definition extends Positional

  /**
    * Represents function definitions. A missing function body implies that
    * this is an imported function.
    */
  case class FuncDef(
      id: Id,
      args: Seq[Decl],
      retTy: Type,
      bodyOpt: Option[Command]
  ) extends Definition
  case class RecordDef(name: Id, fields: Map[Id, Type]) extends Definition {
    fields.foreach({
      case (f, t) =>
        t match {
          case _: TArray => throw ArrayInRecord(name, f, t)
          case _ => ()
        }
    })
  }

  /**
    * An include with the name of the module and function definitions.
    */
  case class Include(
      backends: Map[BackendOption, String],
      defs: Seq[FuncDef]
  ) extends Positional

  case class Decl(id: Id, typ: Type) extends Positional
  case class Prog(
      includes: Seq[Include],
      defs: Seq[Definition],
      decors: Seq[CDecorate],
      decls: Seq[Decl],
      cmd: Command
  ) extends Positional

  /**
    * Define common helper methods implicit classes.
    */
  implicit class RichType(typ: Type) {
    def matchOrError[A](pos: Position, construct: String, exp: String)(
        andThen: PartialFunction[Type, A]
    ): A = {
      val mismatchError: PartialFunction[Type, A] = {
        case _ => throw UnexpectedType(pos, construct, exp, typ)
      }
      andThen.orElse(mismatchError)(typ)
    }
  }
}
