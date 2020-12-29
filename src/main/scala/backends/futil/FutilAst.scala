package fuselang.backend.futil

import fuselang.common.PrettyPrint.Doc
import fuselang.common.CompilerError._
import Doc._

object Futil {

  def emitCompStructure(structs: List[Structure]): Doc = {
    val (cells, connections) = structs.partition(st =>
      st match {
        case _: LibDecl | _: CompDecl => true
        case _ => false
      }
    )
    text("cells") <+> scope(vsep(cells.map(_.doc))) <@>
      text("wires") <+> scope(vsep(connections.map(_.doc)))
  }

  sealed trait Emitable {
    def doc(): Doc
    def emit(): String = this.doc.pretty
  }

  case class CompVar(name: String) extends Emitable with Ordered[CompVar] {
    override def doc(): Doc = text(name)
    def port(port: String): CompPort = CompPort(this, port)
    def addSuffix(suffix: String): CompVar = CompVar(s"$name$suffix")
    override def compare(that: CompVar): Int = {
      this.name.compare(that.name)
    }
  }
  case class PortDef(id: CompVar, width: Int) extends Emitable {
    override def doc(): Doc = parens(text("port") <+> id.doc <+> value(width))
  }

  /**** definition statements *****/
  case class Namespace(name: String, comps: List[NamespaceStatement])
      extends Emitable {
    override def doc(): Doc =
      vsep(comps.map(_.doc))
  }

  /** The statements that can appear in a `define/namespace` construct. */
  sealed trait NamespaceStatement extends Emitable {
    override def doc(): Doc = this match {
      case Import(filename) => text("import") <+> quote(text(filename)) <> semi
      case Component(name, inputs, outputs, structure, control) => {
        text("component") <+>
          text(name) <>
          parens(hsep(inputs.map(_.doc))) <+>
          text("->") <+>
          parens(hsep(outputs.map(_.doc))) <+>
          scope(
            emitCompStructure(structure) <@>
              text("control") <+> scope(control.doc)
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
        id.doc <> dot <> text(name)
      case ThisPort(id) => id.doc
      case HolePort(id, name) =>
        id.doc <> brackets(text(name))
      case ConstantPort(width, value) =>
        text(width.toString) <> text("'d") <> text(value.toString)
    }

    override def compare(that: Port): Int = (this, that) match {
      case (ThisPort(thisId), ThisPort(thatId)) => thisId.compare(thatId)
      case (CompPort(thisId, _), CompPort(thatId, _)) => thisId.compare(thatId)
      case (HolePort(thisId, _), HolePort(thatId, _)) => thisId.compare(thatId)
      case (ConstantPort(_, thisV), ConstantPort(_, thatV)) =>
        thisV.compare(thatV)
      case (ThisPort(_), _) => 1
      case (_, ThisPort(_)) => -1
      case (CompPort(_, _), _) => 1
      case (_, CompPort(_, _)) => -1
      case (ConstantPort(_, _), _) => 1
      case (_, ConstantPort(_, _)) => -1
    }
  }

  case class CompPort(id: CompVar, name: String) extends Port
  case class ThisPort(id: CompVar) extends Port
  case class HolePort(id: CompVar, name: String) extends Port
  case class ConstantPort(width: Int, value: Int) extends Port

  sealed trait Structure extends Emitable with Ordered[Structure] {
    override def doc(): Doc = this match {
      case CompDecl(id, comp) =>
        id.doc <+> equal <+> comp.doc <> semi
      case LibDecl(id, comp) =>
        id.doc <+> equal <+> text("prim") <+> comp.doc <> semi
      case Connect(src, dest, Some(guard)) =>
        dest.doc <+> equal <+> guard.doc <+> text("?") <+> src.doc <> semi
      case Connect(src, dest, None) =>
        dest.doc <+> equal <+> src.doc <> semi
      case Group(id, conns, Some(delay)) =>
        text("group") <+> id.doc <>
          angles(text("\"static\"") <> equal <> text(delay.toString())) <+>
          scope(vsep(conns.map(_.doc)))
      case Group(id, conns, None) =>
        text("group") <+> id.doc <+>
          scope(vsep(conns.map(_.doc)))
    }

    def compare(that: Structure): Int = {
      (this, that) match {
        case (LibDecl(thisId, _), LibDecl(thatId, _)) => thisId.compare(thatId)
        case (CompDecl(thisId, _), CompDecl(thatId, _)) =>
          thisId.compare(thatId)
        case (Group(thisId, _, _), Group(thatId, _, _)) =>
          thisId.compare(thatId)
        case (Connect(thisSrc, thisDest, _), Connect(thatSrc, thatDest, _)) => {
          if (thisSrc.compare(thatSrc) == 0) {
            thisDest.compare(thatDest)
          } else {
            thisSrc.compare(thatSrc)
          }
        }
        case (LibDecl(_, _), _) => -1
        case (_, LibDecl(_, _)) => 1
        case (CompDecl(_, _), _) => -1
        case (_, CompDecl(_, _)) => 1
        case (Group(_, _, _), _) => -1
        case (_, Group(_, _, _)) => 1
      }
    }
  }
  case class LibDecl(id: CompVar, ci: CompInst) extends Structure
  case class CompDecl(id: CompVar, comp: CompVar) extends Structure
  case class Group(
      id: CompVar,
      connections: List[Connect],
      staticDelay: Option[Int]
  ) extends Structure
  object Group {
    def fromStructure(
        id: CompVar,
        structure: List[Structure],
        staticDelay: Option[Int]
    ): (Group, List[Structure]) = {
      val (connections, st) = structure.partitionMap[Connect, Structure](st =>
        st match {
          case c: Connect => Left(c)
          case s => Right(s)
        }
      )
      (this(id, connections, staticDelay), st)
    }
  }
  case class Connect(src: Port, dest: Port, guard: Option[GuardExpr] = None)
      extends Structure
  case class CompInst(id: String, args: List[Int]) extends Emitable {
    override def doc(): Doc = {
      val strList = args.map((x: Int) => text(x.toString()))
      text(id) <> parens(hsep(strList, comma))
    }
  }

  sealed trait GuardExpr extends Emitable {
    override def doc(): Doc = this match {
      case Atom(item) => item.doc
      case And(left, right) => parens(left.doc <+> text("&") <+> right.doc)
      case Or(left, right) => parens(left.doc <+> text("|") <+> right.doc)
      case Not(inner) => text("!") <> inner.doc
    }
  }
  case class Atom(item: Port) extends GuardExpr
  case class And(left: GuardExpr, right: GuardExpr) extends GuardExpr
  case class Or(left: GuardExpr, right: GuardExpr) extends GuardExpr
  case class Not(inner: GuardExpr) extends GuardExpr

  /***** control *****/
  sealed trait Control extends Emitable {
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
    override def doc(): Doc = this match {
      case SeqComp(stmts) =>
        text("seq") <+> scope(vsep(stmts.map(_.doc)))
      case ParComp(stmts) =>
        text("par") <+> scope(vsep(stmts.map(_.doc)))
      case If(port, cond, trueBr, falseBr) =>
        text("if") <+> port.doc <+> text("with") <+>
          cond.doc <+>
          scope(trueBr.doc) <> (
          if (falseBr == Empty)
            emptyDoc
          else
            space <> text("else") <+> scope(falseBr.doc)
        )
      case Invoke(id, ports) => {
        val portsDoc = ports.map(p => p.doc)
        text("invoke") <+> id.doc <> parens(hsep(portsDoc, comma)) <> text("()")
      }
      case While(port, cond, body) =>
        text("while") <+> port.doc <+> text("with") <+>
          cond.doc <+>
          scope(body.doc)
      case Print(_) =>
        throw Impossible("Futil does not support print")
      case Enable(id) => id.doc <> semi
      case Empty => text("empty")
    }
  }
  case class SeqComp(stmts: List[Control]) extends Control
  case class ParComp(stmts: List[Control]) extends Control
  case class If(port: Port, cond: CompVar, trueBr: Control, falseBr: Control)
      extends Control
  case class While(port: Port, cond: CompVar, body: Control) extends Control
  case class Print(id: CompVar) extends Control
  case class Enable(id: CompVar) extends Control
  case class Invoke(id: CompVar, parameters: List[Port]) extends Control
  case object Empty extends Control
}

/** Represents all of the primitives in Futil. */
// extend fixed_point primitive
object Stdlib {
  def register(bitwidth: Int): Futil.CompInst =
    Futil.CompInst("std_reg", List(bitwidth))

  def iterator(bitwidth: Int, start: Int, end: Int, incr: Int): Futil.CompInst =
    Futil.CompInst("std_iterator", List(bitwidth, start, end, incr))

  def constant(bitwidth: Int, v: Int): Futil.CompInst =
    Futil.CompInst("std_const", List(bitwidth, v))

  def op(op: String, bitwidth: Int): Futil.CompInst =
    Futil.CompInst(s"std_$op", List(bitwidth))

  def slice(in: Int, out: Int): Futil.CompInst =
    Futil.CompInst(s"std_slice", List(in, out))

  def identity(bitwidth: Int): Futil.CompInst =
    Futil.CompInst(s"std_id", List(bitwidth))

  def mem_d1(width: Int, size: Int, idxSize: Int): Futil.CompInst =
    Futil.CompInst("std_mem_d1", List(width, size, idxSize))

  def mem_d1_ext(width: Int, size: Int, idxSize: Int): Futil.CompInst =
    Futil.CompInst("std_mem_d1_ext", List(width, size, idxSize))

  def mem_d2(
      width: Int,
      size0: Int,
      size1: Int,
      idxSize0: Int,
      idxSize1: Int
  ): Futil.CompInst =
    Futil.CompInst("std_mem_d2", List(width, size0, size1, idxSize0, idxSize1))

  def mem_d2_ext(
      width: Int,
      size0: Int,
      size1: Int,
      idxSize0: Int,
      idxSize1: Int
  ): Futil.CompInst =
    Futil.CompInst(
      "std_mem_d2_ext",
      List(width, size0, size1, idxSize0, idxSize1)
    )

  def mem_d3(
      width: Int,
      size0: Int,
      size1: Int,
      size2: Int,
      idxSize0: Int,
      idxSize1: Int,
      idxSize2: Int
  ): Futil.CompInst =
    Futil.CompInst(
      "std_mem_d3",
      List(width, size0, size1, size2, idxSize0, idxSize1, idxSize2)
    )

  def mem_d3_ext(
      width: Int,
      size0: Int,
      size1: Int,
      size2: Int,
      idxSize0: Int,
      idxSize1: Int,
      idxSize2: Int
  ): Futil.CompInst =
    Futil.CompInst(
      "std_mem_d3_ext",
      List(width, size0, size1, size2, idxSize0, idxSize1, idxSize2)
    )

    def mem_d4(
        width: Int,
        size0: Int,
        size1: Int,
        size2: Int,
        size3: Int,
        idxSize0: Int,
        idxSize1: Int,
        idxSize2: Int,
        idxSize3: Int
    ): Futil.CompInst =
      Futil.CompInst(
        "std_mem_d4",
        List(width, size0, size1, size2, size3, idxSize0, idxSize1, idxSize2, idxSize3)
      )

    def mem_d4_ext(
        width: Int,
        size0: Int,
        size1: Int,
        size2: Int,
        size3: Int,
        idxSize0: Int,
        idxSize1: Int,
        idxSize2: Int,
        idxSize3: Int
    ): Futil.CompInst =
      Futil.CompInst(
        "std_mem_d4_ext",
        List(width, size0, size1, size2, size3, idxSize0, idxSize1, idxSize2, idxSize3)
      )

  def sqrt(): Futil.CompInst =
    Futil.CompInst("std_sqrt", List())

  def exp(): Futil.CompInst =
    Futil.CompInst("std_exp", List())
  
  // Extended AST to support fixed point constant and operations
  def fixed_point(
      width: Int,
      int_bit: Int,
      frac_bit: Int,
      value1: Int,
      value2: Int 
  ): Futil.CompInst =
    Futil.CompInst("fixed_p_std_const", List(width, int_bit, frac_bit, value1, value2))

  def fxd_p_op(op: String,
      width: Int,
      int_bit: Int,
      frac_bit: Int
  ): Futil.CompInst =
    Futil.CompInst(s"fixed_p_std_$op", List(width, int_bit, frac_bit))

  def diff_width_add (
      width1: Int,
      width2: Int,
      int_width1: Int, 
      fract_width1: Int,
      int_width2: Int,
      fract_width2: Int,
      out_width: Int
  ): Futil.CompInst = 
    Futil.CompInst("fixed_p_std_add_dbit", List(width1, width2, int_width1, fract_width1,
                      int_width2, fract_width2, out_width))

    def sdiff_width_add (
      width1: Int,
      width2: Int,
      int_width1: Int, 
      fract_width1: Int,
      int_width2: Int,
      fract_width2: Int,
      out_width: Int
  ): Futil.CompInst = 
    Futil.CompInst("sfixed_p_std_add_dbit", List(width1, width2, int_width1, fract_width1,
                      int_width2, fract_width2, out_width))

  def s_op(op: String, bitwidth: Int): Futil.CompInst =
    Futil.CompInst(s"std_s$op", List(bitwidth))

  def fxd_p_sop(op: String,
      width: Int,
      int_bit: Int,
      frac_bit: Int
  ): Futil.CompInst =
    Futil.CompInst(s"fixed_p_std_s$op", List(width, int_bit, frac_bit))

  val staticTimingMap: Map[String, Option[Int]] = Map(
    "sqrt" -> Some(17),
    "mult" -> Some(3),
    "exp" -> None,
    "div" -> None,
    "mod" -> None
  )
}
