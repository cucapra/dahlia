package fuselang.backend

import fuselang.Syntax._
// import fuselang.Errors._
import fuselang.Utils._

private object Dot {

  type Name = String

  sealed trait Emittable {
    def emit(): String = ""
  }

  sealed trait Prop extends Emittable
  case class Label(str: String) extends Prop {
    override def emit(): String = s"label=$str"
  }

  sealed trait Node extends Emittable
  case class PropNode(name: Name, props: List[Prop]) extends Node {
    override def emit(): String = {
      val str = props.foldLeft("")((acc, e) => acc concat e.emit)
      s"  $name [$str];\n"
    }
  }
  case class EdgeNode(src: Name, tar: Name) extends Node
  case class DirectedEdgeNode(src: Name, tar: Name) extends Node

  class Prog extends Emittable {
    var nodes: List[Node] = List()
    def append(n: Node) = nodes = n :: nodes
    override def emit() =
      nodes.foldLeft("")((acc, e) => acc concat e.emit)
  }
}

private object Vizualization {

  def preamble = {
    """
digraph G {
  node [shape=plain];
"""
  }

  def postamble = "}"

  def emitDefn(defn: Definition): String = defn match {
    case FuncDef(_, _, _) => ""
    case RecordDef(_, _) => ""
  }

  def emitDecl(decl: Decl): String = {
    val p = new Dot.Prog
    p.append(
      Dot.PropNode(
        decl.id.toString(),
        List(Dot.Label("hi"))))
    p.emit
  }

  def emit(p: Prog, c: Config): String = {
    if (c.srcFile == "cat") println("cat") // this is just to get rid of a warning
    preamble concat
    p.defs.foldLeft("")((str, defn) => str concat emitDefn(defn)) concat
    p.decls.foldLeft("")((str, decl) => str concat emitDecl(decl)) concat
    postamble
  }
}

case object VizBackend extends Backend {
  def emitProg(p: Prog, c: Config) = Vizualization.emit(p, c)
}
