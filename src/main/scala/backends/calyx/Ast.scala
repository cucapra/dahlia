package fuselang.backend.calyx

import scala.math.BigInt
import fuselang.common.PrettyPrint.Doc
import fuselang.common.CompilerError._
import fuselang.Utils.RichOption
import Doc._

object Calyx {
  def emitCompStructure(structs: List[Structure]): Doc = {
    val (cells, connections) = structs.partition(st =>
      st match {
        case _: Cell => true
        case _ => false
      }
    )
    text("cells") <+> scope(vsep(cells.map(_.doc()))) <@>
      text("wires") <+> scope(vsep(connections.map(_.doc())))
  }

  sealed trait Emitable {
    def doc(): Doc
    def emit(): String = this.doc().pretty
  }

  /** A variable representing the name of a component. **/
  case class CompVar(name: String) extends Emitable with Ordered[CompVar] {
    override def doc(): Doc = text(name)
    def port(port: String): CompPort = CompPort(this, port)
    def addSuffix(suffix: String): CompVar = CompVar(s"$name$suffix")
    override def compare(that: CompVar): Int = {
      this.name.compare(that.name)
    }
  }
  case class PortDef(
      id: CompVar,
      width: Int,
      attrs: List[(String, Int)] = List()
  ) extends Emitable {
    override def doc(): Doc = {
      val attrDoc = hsep(attrs.map({
        case (attr, v) => text(s"@${attr}") <> parens(text(v.toString()))
      })) <> (if (attrs.isEmpty) emptyDoc else space)
      attrDoc <> id.doc() <> colon <+> value(width)
    }
  }

  /** The statements that can appear at the top-level. */
  sealed trait NamespaceStatement extends Emitable {
    override def doc(): Doc = this match {
      case Import(filename) => text("import") <+> quote(text(filename)) <> semi
      case Component(name, inputs, outputs, structure, control) => {
        text("component") <+>
          text(name) <>
          parens(commaSep(inputs.map(_.doc()))) <+>
          text("->") <+>
          parens(commaSep(outputs.map(_.doc()))) <+>
          scope(
            emitCompStructure(structure) <@>
              text("control") <+> scope(control.doc())
          )
      }
    }
  }

  case class Import(filename: String) extends NamespaceStatement
  case class Component(
      name: String,
      inputs: List[PortDef],
      outputs: List[PortDef],
      structure: List[Structure],
      control: Control
  ) extends NamespaceStatement

  /***** structure *****/
  sealed trait Port extends Emitable with Ordered[Port] {
    override def doc(): Doc = this match {
      case CompPort(id, name) =>
        id.doc() <> dot <> text(name)
      case ThisPort(id) => id.doc()
      case HolePort(id, name) =>
        id.doc() <> brackets(text(name))
      case ConstantPort(width, value) =>
        text(width.toString) <> text("'d") <> text(value.toString)
    }

    override def compare(that: Port): Int = (this, that) match {
      case (ThisPort(thisId), ThisPort(thatId)) => thisId.compare(thatId)
      case (CompPort(thisId, _), CompPort(thatId, _)) => thisId.compare(thatId)
      case (HolePort(thisId, _), HolePort(thatId, _)) => thisId.compare(thatId)
      case (ConstantPort(_, thisV), ConstantPort(_, thatV)) =>
        thisV.compare(thatV)
      case (_: ThisPort, _) => 1
      case (_, _: ThisPort) => -1
      case (_: CompPort, _) => 1
      case (_, _: CompPort) => -1
      case (_: ConstantPort, _) => 1
      case (_, _: ConstantPort) => -1
    }

    def isHole(): Boolean = this match {
      case _: HolePort => true
      case _ => false
    }

    def isConstant(value: Int, width: Int) = this match {
      case ConstantPort(v, w) if v == value && w == width => true
      case _ => false
    }
  }

  case class CompPort(id: CompVar, name: String) extends Port
  case class ThisPort(id: CompVar) extends Port
  case class HolePort(id: CompVar, name: String) extends Port
  case class ConstantPort(width: Int, value: BigInt) extends Port

  sealed trait Structure extends Emitable with Ordered[Structure] {
    override def doc(): Doc = this match {
      case Cell(id, comp, ref, attrs) => {
        val attrDoc =
          hsep(
            attrs
              .map({
                case (attr, v) =>
                  text("@") <> text(attr) <> parens(text(v.toString()))
              })
          ) <> (if (attrs.isEmpty) emptyDoc else space)

        attrDoc <> (if (ref) text("ref") <> space else emptyDoc) <>
          id.doc() <+> equal <+> comp.doc() <> semi
      }
      case Assign(src, dest, True) =>
        dest.doc() <+> equal <+> src.doc() <> semi
      case Assign(src, dest, guard) =>
        dest.doc() <+> equal <+> guard.doc() <+> text("?") <+> src.doc() <> semi
      case Group(id, conns, delay, comb) =>
        (if (comb) text("comb ") else emptyDoc) <>
          text("group") <+> id.doc() <>
          (if (delay.isDefined)
             angles(text("\"static\"") <> equal <> text(delay.get.toString()))
           else emptyDoc) <+>
          scope(vsep(conns.map(_.doc())))
    }

    def compare(that: Structure): Int = {
      (this, that) match {
        case (Cell(thisId, _, _, _), Cell(thatId, _, _, _)) =>
          thisId.compare(thatId)
        case (Group(thisId, _, _, _), Group(thatId, _, _, _)) =>
          thisId.compare(thatId)
        case (Assign(thisSrc, thisDest, _), Assign(thatSrc, thatDest, _)) => {
          if (thisSrc.compare(thatSrc) == 0) {
            thisDest.compare(thatDest)
          } else {
            thisSrc.compare(thatSrc)
          }
        }
        case (_: Cell, _) => -1
        case (_, _: Cell) => 1
        case (_: Group, _) => -1
        case (_, _: Group) => 1
      }
    }
  }
  case class Cell(
      id: CompVar,
      ci: CompInst,
      ref: Boolean,
      attributes: List[(String, Int)]
  ) extends Structure
  case class Group(
      id: CompVar,
      connections: List[Assign],
      staticDelay: Option[Int],
      // True if the group is combinational
      combinationa: Boolean
  ) extends Structure
  case class Assign(src: Port, dest: Port, guard: GuardExpr = True)
      extends Structure

  object Group {
    def fromStructure(
        id: CompVar,
        structure: List[Structure],
        staticDelay: Option[Int]
    ): (Group, List[Structure]) = {
      val (connections, st) = structure.partitionMap[Assign, Structure](st =>
        st match {
          case c: Assign => Left(c)
          case s => Right(s)
        }
      )
      val doneAssign = connections
        .find(assign => assign.dest.isHole())
        .getOrThrow(Impossible("Group does not have a done hole"))

      // If this is a combinational group, remove the group done assignment
      val isComb = doneAssign.guard == True && doneAssign.src.isConstant(1, 1)
      val conns = if (isComb) {
        connections.filter(assign => !assign.dest.isHole())
      } else {
        connections
      }
      (this(id, conns, if (isComb) None else staticDelay, isComb), st)
    }
  }

  case class CompInst(id: String, args: List[BigInt]) extends Emitable {
    override def doc(): Doc = {
      val strList = args.map((x: BigInt) => text(x.toString()))
      text(id) <> parens(hsep(strList, comma))
    }
  }

  sealed trait GuardExpr extends Emitable {
    override def doc(): Doc = this match {
      case Atom(item) => item.doc()
      case And(left, right) => parens(left.doc() <+> text("&") <+> right.doc())
      case Or(left, right) => parens(left.doc() <+> text("|") <+> right.doc())
      case Not(inner) => text("!") <> inner.doc()
      case True => emptyDoc
    }
  }
  case class Atom(item: Port) extends GuardExpr
  object Atom {
    def apply(item: Port): GuardExpr = item match {
      case ConstantPort(1, v) if v == 1 => True
      case _ => new Atom(item)
    }
  }
  case class And(left: GuardExpr, right: GuardExpr) extends GuardExpr
  case class Or(left: GuardExpr, right: GuardExpr) extends GuardExpr
  case class Not(inner: GuardExpr) extends GuardExpr
  case object True extends GuardExpr

  /***** control *****/
  sealed trait Control extends Emitable {
    var attributes = Map[String, Int]()

    def seq(c: Control): Control = (this, c) match {
      case (Empty, c) => c
      case (c, Empty) => c
      case (seq0: SeqComp, seq1: SeqComp) => SeqComp(seq0.stmts ++ seq1.stmts)
      case (seq: SeqComp, _) => SeqComp(seq.stmts ++ List(c))
      case (_, seq: SeqComp) => SeqComp(this :: seq.stmts)
      case _ => SeqComp(List(this, c))
    }

    def par(c: Control): Control = (this, c) match {
      case (Empty, c) => c
      case (c, Empty) => c
      case (par0: ParComp, par1: ParComp) => ParComp(par0.stmts ++ par1.stmts)
      case (par0: ParComp, par1) => ParComp(par0.stmts ++ List(par1))
      case (par0, par1: ParComp) => ParComp(par0 :: par1.stmts)
      case _ => ParComp(List(this, c))
    }

    def attributesDoc(): Doc =
      if (this.attributes.isEmpty) {
        emptyDoc
      } else {
        hsep(attributes.map({
          case (attr, v) =>
            text(s"@$attr") <> parens(text(v.toString()))
        })) <> space
      }

    override def doc(): Doc = {
      val controlDoc = this match {
        case SeqComp(stmts) =>
          text("seq") <+> scope(vsep(stmts.map(_.doc())))
        case ParComp(stmts) =>
          text("par") <+> scope(vsep(stmts.map(_.doc())))
        case If(port, cond, trueBr, falseBr) =>
          text("if") <+> port.doc() <+> text("with") <+>
            cond.doc() <+>
            scope(trueBr.doc()) <> (
            if (falseBr == Empty)
              emptyDoc
            else
              space <> text("else") <+> scope(falseBr.doc())
          )
        case While(port, cond, body) =>
          text("while") <+> port.doc() <+> text("with") <+>
            cond.doc() <+>
            scope(body.doc())
        case Enable(id) => id.doc() <> semi
        case Invoke(id, refCells, inConnects, outConnects) => {
          val cells =
            if (refCells.isEmpty)
              emptyDoc
            else
              brackets(commaSep(refCells.map({
                case (param, cell) => text(param) <> equal <> cell.doc()
              })))
          val inputDefs = inConnects.map({
            case (param, arg) => text(param) <> equal <> arg.doc()
          })
          val outputDefs = outConnects.map({
            case (param, arg) => text(param) <> equal <> arg.doc()
          })
          text("invoke") <+> id.doc() <>
            cells <>
            parens(commaSep(inputDefs)) <>
            parens(commaSep(outputDefs)) <> semi
        }
        case Empty => text("empty")
      }
      attributesDoc() <> controlDoc
    }
  }
  case class SeqComp(stmts: List[Control]) extends Control
  case class ParComp(stmts: List[Control]) extends Control
  case class If(port: Port, cond: CompVar, trueBr: Control, falseBr: Control)
      extends Control
  case class While(port: Port, cond: CompVar, body: Control) extends Control
  case class Enable(id: CompVar) extends Control
  case class Invoke(
      id: CompVar,
      refCells: List[(String, CompVar)],
      inConnects: List[(String, Port)],
      outConnects: List[(String, Port)]
  ) extends Control
  case object Empty extends Control
}

/** Construct primitives in Calyx. */
object Stdlib {
  def register(name: Calyx.CompVar, width: Int) =
    Calyx.Cell(
      name,
      Calyx.CompInst("std_reg", List(width)),
      false,
      List()
    )

  def constant(bitwidth: Int, v: BigInt): Calyx.CompInst =
    Calyx.CompInst("std_const", List(bitwidth, v))

  def binop(op: String, bitwidth: Int, signed: Boolean): Calyx.CompInst =
    Calyx.CompInst(
      s"std_${if (signed) "s" else ""}$op",
      List(bitwidth)
    )

  def slice(in: Int, out: Int): Calyx.CompInst =
    Calyx.CompInst(s"std_slice", List(in, out))

  def pad(in: Int, out: Int): Calyx.CompInst =
    Calyx.CompInst(s"std_pad", List(in, out))

  // A fixed point operator
  def fixed_point_binop(
      op: String,
      width: Int,
      int_width: Int,
      frac_width: Int,
      signed: Boolean
  ): Calyx.CompInst =
    Calyx.CompInst(
      s"std_fp_${(if (signed) "s" else "")}$op",
      List(width, int_width, frac_width)
    )

  val staticTimingMap: Map[String, Int] = Map(
    "mult" -> 3
  )
}
