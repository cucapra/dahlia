package fuselang

import scala.util.parsing.input.Positional
import scribe._

object Syntax {

  import Errors._

  /** Makes all positionals logable by scribe */
  implicit object PositionalLoggable extends Loggable[(String,Positional)] {
    override def apply(value: (String, Positional)) = {
      new output.TextOutput(s"${value._1}\n${value._2.pos.longString}")
    }
  }

  case class Id(v: String) extends Positional {
    var typ: Option[Type] = None;
    override def toString = s"$v"
  }

  sealed trait Type extends Positional {
    override def toString = this match {
      case _: TVoid => "void"
      case _: TBool => "bool"
      case _: TFloat => "float"
      case TSizedInt(l) => s"int$l"
      case TStaticInt(s) => s"static($s)"
      case TArray(t, dims) =>
        s"$t" + dims.foldLeft("")({ case (acc, (d, b)) => s"$acc[$d bank $b]" })
      case TIndex(s, d) => s"idx($s, $d)"
      case TFun(args) => s"${args.mkString("->")} -> void"
      case TRecType(n, fs) => {
        val fields = fs.toList.map({ case (id, typ) => s"$id: $typ"}).mkString(";")
        s"$n($fields)"
      }
      case TAlias(n) => n.toString
    }
  }
  // Types that can be upcast to Ints
  sealed trait IntType
  case class TSizedInt(len: Int) extends Type with IntType
  case class TStaticInt(v: Int) extends Type with IntType
  case class TIndex(static: (Int, Int), dynamic: (Int, Int)) extends Type with IntType {
    val maxVal: Int = static._2 * dynamic._2
  }
  // Use case class instead of case object to get unique positions
  case class TVoid() extends Type
  case class TBool() extends Type
  case class TFloat() extends Type
  case class TFun(args: List[Type]) extends Type
  case class TRecType(name: Id, fields: Map[Id, Type]) extends Type
  case class TAlias(name: Id) extends Type
  case class TArray(typ: Type, dims: List[(Int, Int)]) extends Type

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

    def toFun: Option[(Int, Int) => Int] = this match {
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
    dims.foreach({ case (_, w, s) => {
      if (w != s) {
        throw MalformedShrink(this, w, s)
      }
    }})
  }

  sealed trait Command extends Positional
  case class CPar(c1: Command, c2: Command) extends Command
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CLet(id: Id, var typ: Option[Type], e: Expr) extends Command
  case class CView(id: Id, kind: ViewType) extends Command
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
}
