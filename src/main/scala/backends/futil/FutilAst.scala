package fuselang.backend.futil

import fuselang.backend.PrettyPrint.Doc

object Futil {

  sealed trait Emitable {
    def doc(): Doc
    def emit(): String = this.doc.pretty
  }

  case class CompVar(name: String) extends Emitable {
    override def doc(): Doc = Doc.text(name)
    def @@(port: String): CompPort = CompPort(this, port)
    def ^^(suffix: String): CompVar = CompVar(s"$name$suffix")
  }
  case class PortDef(id: CompVar, width: Int) extends Emitable {
    override def doc(): Doc = Doc.parens(id.doc <+> Doc.value(width))
  }

  /**** definition statements *****/
  case class Namespace(name: String, comps: List[Component]) extends Emitable {
    override def doc(): Doc =
      Doc.parens(
        Doc.text("define/namespace") <+> Doc.text(name) <@> Doc.vsep(
          comps.map(_.doc)
        )
      )
  }
  case class Component(
      name: String,
      inputs: List[PortDef],
      outputs: List[PortDef],
      structure: List[Structure],
      control: Control
  ) extends Emitable {
    override def doc(): Doc = Doc.parens(
      Doc.text("define/component")
        <+> Doc.text(name)
        <+> Doc.parens(Doc.hsep(inputs.map(_.doc)))
        <+> Doc.parens(Doc.hsep(outputs.map(_.doc)))
        <@> Doc.parens(Doc.vsep(structure.map(_.doc)))
        <@> control.doc
    )
  }

  /***** structure *****/
  sealed trait Port extends Emitable {
    override def doc(): Doc = this match {
      case CompPort(id, name) =>
        Doc.parens(Doc.text("@") <+> id.doc <+> Doc.text(name))
      case ThisPort(id) =>
        Doc.parens(Doc.text("@") <+> Doc.text("this") <+> id.doc)
    }
  }

  case class CompPort(id: CompVar, name: String) extends Port
  case class ThisPort(id: CompVar) extends Port

  sealed trait Structure extends Emitable {
    override def doc(): Doc = this match {
      case CompDecl(id, comp) =>
        Doc.brackets(Doc.text("new") <+> id.doc <+> comp.doc)
      case LibDecl(id, ci) =>
        Doc.brackets(
          Doc.text("new-std") <+> id.doc <+> ci.doc
        )
      case Connect(src, dest) =>
        Doc.brackets(Doc.text("->") <+> src.doc <+> dest.doc)
    }
  }
  case class CompDecl(id: CompVar, comp: CompVar) extends Structure
  case class LibDecl(id: CompVar, ci: CompInst) extends Structure
  case class Connect(src: Port, dest: Port) extends Structure

  case class CompInst(id: String, args: List[Value]) extends Emitable {
    override def doc(): Doc =
      Doc.parens(Doc.text(id) <+> Doc.hsep(args.map(_.doc)))
  }

  sealed trait Value extends Emitable {
    override def doc(): Doc = this match {
      case Num(v)  => Doc.value(v)
      case VNone() => Doc.text("#f")
    }
  }
  case class Num(v: Any) extends Value
  case class VNone() extends Value

  /***** control *****/
  sealed trait Control extends Emitable {
    def ---(c: Control): Control = (this, c) match {
      case (seq0: SeqComp, seq1: SeqComp) => SeqComp(seq0.stmts ++ seq1.stmts)
      case (seq: SeqComp, _)              => SeqComp(seq.stmts ++ List(c))
      case (_, seq: SeqComp)              => SeqComp(this :: seq.stmts)
      case _                              => SeqComp(List(this, c))
    }

    def <:>(c: Control): Control = (this, c) match {
      case (par0: ParComp, par1: ParComp) => ParComp(par0.stmts ++ par1.stmts)
      case (par0: ParComp, par1)          => ParComp(par0.stmts ++ List(par1))
      case (par0, par1: ParComp)          => ParComp(par0 :: par1.stmts)
      case _                              => ParComp(List(this, c))
    }
    override def doc(): Doc = this match {
      case SeqComp(stmts) =>
        Doc.parens(Doc.text("seq") <@> Doc.vsep(stmts.map(_.doc)))
      case ParComp(stmts) =>
        Doc.parens(Doc.text("par") <@> Doc.vsep(stmts.map(_.doc)))
      case If(cond, trueBr, falseBr) =>
        Doc.parens(Doc.text("if") <+> cond.doc <@> trueBr.doc <@> falseBr.doc)
      case Ifen(cond, trueBr, falseBr) =>
        Doc.parens(Doc.text("ifen") <+> cond.doc <@> trueBr.doc <@> falseBr.doc)
      case While(cond, body) =>
        Doc.parens(Doc.text("ifen") <+> cond.doc <@> body.doc)
      case Print(id) =>
        Doc.parens(Doc.text("print") <+> id.doc)
      case Enable(ids) =>
        Doc.parens(Doc.text("enable") <+> Doc.hsep(ids.map(_.doc)))
      case Disable(ids) =>
        Doc.parens(Doc.text("disable") <+> Doc.hsep(ids.map(_.doc)))
      case Empty() =>
        Doc.parens(Doc.text("empty"))
    }
  }
  case class SeqComp(stmts: List[Control]) extends Control
  case class ParComp(stmts: List[Control]) extends Control
  case class If(cond: Port, trueBr: Control, falseBr: Control) extends Control
  case class Ifen(cond: Port, trueBr: Control, falseBr: Control) extends Control
  case class While(cond: Port, body: Control) extends Control
  case class Print(id: CompVar) extends Control
  case class Enable(ids: List[CompVar]) extends Control {
    def ++(en: Enable): Enable =
      Enable(ids ++ (en.ids)) // XXX(sam) make distincts
  }
  case class Disable(ids: List[CompVar]) extends Control
  case class Empty() extends Control
}

object Stdlib {

  def constant(v: Futil.Value): Futil.CompInst =
    Futil.CompInst("const", List(v))

  def constant(v: String): Futil.CompInst =
    Futil.CompInst("const", List(Futil.Num(v)))

  def memory(dims: List[Int]): Futil.CompInst =
    Futil.CompInst("memory", dims.map(Futil.Num(_)))

  def register(): Futil.CompInst =
    Futil.CompInst("comp/reg", List())

  def op(op: String): Futil.CompInst =
    Futil.CompInst(op, List())

  def identity(): Futil.CompInst =
    Futil.CompInst("comp/id", List())

  def sqrt(): Futil.CompInst =
    Futil.CompInst("comp/sqrt", List())

  def iterator(): Futil.CompInst =
    Futil.CompInst("comp/iterator", List())

}
