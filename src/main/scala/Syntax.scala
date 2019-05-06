package fuselang

import scala.util.parsing.input.{Positional, Position}

object Syntax {

  import Errors._

  case class Id(v: String) extends Positional {
    var typ: Option[Type] = None;
    override def toString = s"$v"
  }

  // Capabilities for read/write
  sealed trait Capability
  case object Read extends Capability
  case object Write extends Capability

  sealed trait Type extends Positional {
    override def toString = this match {
      case _: TVoid => "void"
      case _: TBool => "bool"
      case _: TFloat => "float"
      case _: TDouble => "double"
      case TSizedInt(l) => s"bit<$l>"
      case TStaticInt(s) => s"static($s)"
      case TArray(t, dims) =>
        s"$t" + dims.foldLeft("")({ case (acc, (d, b)) => s"$acc[$d bank $b]" })
      case TIndex(s, d) => s"idx($s, $d)"
      case TFun(args) => s"${args.mkString("->")} -> void"
      case TRecType(n, _) => s"$n"
      case TAlias(n) => n.toString
    }
  }
  // Types that can be upcast to Ints
  sealed trait IntType
  case class TSizedInt(len: Int) extends Type with IntType
  case class TStaticInt(v: Int) extends Type with IntType
  case class TIndex(static: (Int, Int), dynamic: (Int, Int)) extends Type with IntType {
    // Our ranges are represented as s..e with e excluded from the range.
    // Therefore, the maximum value is one than the product of the interval ends.
    val maxVal: Int = static._2 * dynamic._2 - 1
  }
  // Use case class instead of case object to get unique positions
  case class TVoid() extends Type
  case class TBool() extends Type
  case class TFloat() extends Type
  case class TDouble() extends Type
  case class TFun(args: List[Type]) extends Type
  case class TRecType(name: Id, fields: Map[Id, Type]) extends Type
  case class TAlias(name: Id) extends Type
  case class TArray(typ: Type, dims: List[(Int, Int)]) extends Type {
    dims.zipWithIndex.foreach({ case ((len, bank), dim) =>
      if (bank > len || len % bank != 0) {
        throw MalformedType(s"Dimension $dim of TArray is malformed. Length $len, banking factor $bank. Full type $this")
      }
    })
  }

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
  }
  // Equality ops
  sealed trait EqOp
  case class OpEq() extends BOp with EqOp
  case class OpNeq() extends BOp with EqOp
  // Comparison ops
  sealed trait CmpOp
  case class OpLt() extends BOp with CmpOp
  case class OpGt() extends BOp with CmpOp
  case class OpLte() extends BOp with CmpOp
  case class OpGte() extends BOp with CmpOp
  // Boolean ops
  sealed trait BoolOp
  case class OpAnd() extends BOp with BoolOp
  case class OpOr() extends BOp with BoolOp
  // integer/float ops
  sealed trait NumOp
  case class OpAdd() extends BOp with NumOp
  case class OpSub() extends BOp with NumOp
  case class OpMul() extends BOp with NumOp
  case class OpDiv() extends BOp with NumOp
  case class OpMod() extends BOp with NumOp
  // Bit ops
  sealed trait BitOp
  case class OpLsh() extends BOp with BitOp
  case class OpRsh() extends BOp with BitOp
  case class OpBOr() extends BOp with BitOp
  case class OpBAnd() extends BOp with BitOp
  case class OpBXor() extends BOp with BitOp

  sealed trait Expr extends Positional {
    def isLVal = this match {
      case _:EVar | _:EArrAccess => true
      case _ => false
    }
    var typ: Option[Type] = None
  }
  case class EInt(v: Int, base: Int = 10) extends Expr
  case class EFloat(f: Float) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr
  case class EArrAccess(id: Id, idxs: List[Expr]) extends Expr
  case class ERecAccess(rec: Expr, fieldName: Id) extends Expr
  case class ERecLiteral(fields: Map[Id, Expr]) extends Expr
  case class EApp(func: Id, args: List[Expr]) extends Expr
  case class EVar(id: Id) extends Expr
  case class ECast(e: Expr, castType: Type) extends Expr

  sealed trait CRange extends Positional {
    val iter: Id;
    val unroll: Int;
  }
  case class StaticRange(
    iter: Id,
    start: Int,
    end: Int,
    unroll: Int) extends CRange {
    def idxType: TIndex = {
      if ((end - start) % unroll != 0) {
        throw UnrollRangeError(this.pos, end - start, unroll)
      } else {
        TIndex((0, unroll), (start/unroll, end/unroll))
      }
    }
  }
  case class DynamicRange(
    iter:Id,
    start: Expr,
    end: Expr,
    step: Expr,
    unroll: Int) extends CRange {
      if (unroll != 1) throw InvalidDynamicUnroll(this.pos, unroll)
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
  case class View(suffix: Suffix,
                  prefix: Option[Int],
                  shrink: Option[Int]) extends Positional

  sealed trait Command extends Positional
  case class CPar(c1: Command, c2: Command) extends Command
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CLet(id: Id, var typ: Option[Type], e: Expr) extends Command
  case class CView(id: Id, arrId: Id, dims: List[View]) extends Command
  case class CSplit(id: Id, arrId: Id, factors: List[Int]) extends Command
  case class CIf(cond: Expr, cons: Command, alt: Command) extends Command
  case class CFor(range: CRange, par: Command, combine: Command) extends Command
  case class CWhile(cond: Expr, body: Command) extends Command
  case class CUpdate(lhs: Expr, rhs: Expr) extends Command {
    if (lhs.isLVal == false) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CReduce(rop: ROp, lhs: Expr, rhs: Expr) extends Command {
    if (lhs.isLVal == false) throw UnexpectedLVal(lhs, "reduction")
  }
  case class CExpr(exp: Expr) extends Command
  case object CEmpty extends Command

  sealed trait Definition extends Positional
  /**
   * Represents function definitions. A missing function body implies that
   * this is an extern function.
   */
  case class FuncDef(id: Id, args: List[Decl], bodyOpt: Option[Command]) extends Definition
  case class RecordDef(name: Id, fields: Map[Id, Type]) extends Definition {
    fields.foreach({ case (f, t) => t match {
      case _:TArray => throw ArrayInRecord(name, f, t)
      case _ => ()
    }})
  }

  /**
   * An include with the name of the module and external function definitions.
   */
  case class Include(name: String, defs: List[FuncDef]) extends Positional

  case class Decl(id: Id, typ: Type) extends Positional
  case class Prog(
    includes: List[Include],
    defs: List[Definition],
    decls: List[Decl],
    cmd: Command) extends Positional

  /**
   * Define common helper methods implicit classes.
   */
  implicit class RichType(typ: Type) {
    def matchOrError[A](pos: Position, construct: String, exp: String)
                       (andThen: PartialFunction[Type, A]): A = {
      val mismatchError: PartialFunction[Type, A] = {
        case _ => throw UnexpectedType(pos, construct, exp, typ)
      }
      andThen.orElse(mismatchError)(typ)
    }
  }

  implicit class RichBop(bop: BOp) {
    def toFun: Option[(Int, Int) => Int] = bop match {
      case _:OpAdd => Some(_ + _)
      case _:OpMul => Some(_ * _)
      case _:OpDiv => Some(_ / _)
      case _:OpSub => Some(_ - _)
      case _:OpBOr => Some(_ | _)
      case _:OpBAnd => Some(_ & _)
      case _:OpBXor => Some(_ ^ _)
      case _ => None
    }
  }
}

