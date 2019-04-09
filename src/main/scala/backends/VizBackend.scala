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
  case class DirectedEdgeNode(src: Name, tar: Name) extends Node {
    override def emit(): String = s"$src -> $tar;"
  }

  class Table(width: Int) extends Emittable {

    private case class Item(name: String, value: String, width: Int)
    private var table: List[List[Item]] = Nil

    def addFullRow(row: (String, String)) = {
      table = List(Item(row._1, row._2, width)) :: table
    }

    def addRow(row: List[(String,String)]) = {
      assert(row.length == width)
      table = (row.map { case (name, value) => Item(name, value, 1) }) :: table
    }

    override def emit(): String = {
      val rows = table.reverseMap { row =>
        val s = row.map {
          case Item(name, value, w) =>
            s"<td port='$name' colspan='$w'>$value</td>"
        }.mkString(" ")
        s"<tr>$s</tr>"
      }.mkString("\n")
      s"""<<table border="0" cellborder="1" cellspacing="0">$rows</table>>"""
    }
  }

  object Table {
    def mkVarTable(name: String, typ: String, value: Option[String]) = {
      val t = new Table(1)
      t.addFullRow((name, name))
      t.addFullRow((typ, typ))
      value.map { s => t.addFullRow(("value", s)) }
      t.emit
    }

    def mkArrTable(name: String, typ: String, nbanks: Int) = {
      val t = new Table(nbanks)
      t.addFullRow((name, name))
      t.addFullRow((typ, typ))
      t.addRow((0 until nbanks).map { i => (s"bank$i", s"$i") }.toList)
      t.emit
    }
  }

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
    val table = decl.typ match {
      case TArray(typ, dims) => Dot.Table.mkArrTable(
        decl.id.toString(),
        typ.toString(),
        dims.foldLeft(0)((acc, e) => acc + e._2) // e._2 is banking factor
      )
      case typ => Dot.Table.mkVarTable(
        decl.id.toString(),
        typ.toString(),
        None)
    }
    val p = new Dot.Prog
    p.append(
      Dot.PropNode(decl.id.toString(), List(Dot.Label(table))))
    p.emit
  }

  def emitExpr(expr: Expr, target: String): (String, Option[String]) = {
    def exprNode(expr: Expr): Option[String] = expr match {
      case EInt(_, _) | EFloat(_) | EBool(_) => None
      case EBinop(_, e1, e2) => (exprNode(e1), exprNode(e2)) match {
        case (Some(s1), Some(s2)) => Some(s"$s1\n$s2")
        case (Some(x), _) => Some(x)
        case (None, Some(x)) => Some(x)
        case (None, None) => None
      }
      case EArrAccess(_, _) => None
      case ERecAccess(_, _) => None
      case ERecLiteral(_) => None
      case EApp(_, _) => None
      case EVar(x) => Some(Dot.DirectedEdgeNode(x.toString(), target).emit)
    }
    (s"$expr", exprNode(expr))
  }

  private var exprIdx = 0
  def newExprName() = {
    val s = s"expr$exprIdx"
    exprIdx += 1
    s
  }

  def emitCmd(cmd: Command): String = cmd match {
    case CPar(c1, c2) => emitCmd(c1) concat emitCmd(c2)
    case CSeq(_, _) => ""
    case CLet(id, _, e) => {
      val typString = e.typ match {
        case None => "no type"
        case Some(t) => t.toString()
      }
      val (exprSt, node) = emitExpr(e, id.toString()) match {
        case (s, None) => (s, "")
        case (s, Some(node)) => (s, node)
      }
      val table = Dot.Table.mkVarTable(
        id.toString(),
        typString,
        Some(exprSt))
      Dot.PropNode(id.toString(), List(Dot.Label(table))).emit() concat node
    }
    case CView(_, _) => ""
    case CIf(_, _, _) => ""
    case CFor(_, _, _) => ""
    case CWhile(_, _) => ""
    case CUpdate(_, _) => ""
    case CReduce(_, _, _) => ""
    case CExpr(e) => emitExpr(e, newExprName()) match {
      case (_, None) => ""
      case (_, Some(node)) => node
    }
    case CEmpty => ""
  }

  def emit(p: Prog, c: Config): String = {
    if (c.srcFile == "cat") println("cat") // this is just to get rid of a warning
    preamble concat
    p.defs.foldLeft("")((str, defn) => str concat emitDefn(defn)) concat
    p.decls.foldLeft("")((str, decl) => str concat emitDecl(decl)) concat
    emitCmd(p.cmd) concat
    postamble
  }
}

case object VizBackend extends Backend {
  def emitProg(p: Prog, c: Config) = Vizualization.emit(p, c)
}
