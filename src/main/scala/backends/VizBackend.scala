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
  case class EdgeNode(src: Name, tar: Name) extends Node {
    override def emit(): String = s"$src -- $tar";
  }
  case class DirectedEdgeNode(src: Name, tar: Name) extends Node {
    override def emit(): String = s"$src -> $tar;"
  }
  case class RangeNode(name: String, range: CRange) extends Node {
    val i = range.iter
    val start = range.s
    val end = range.e
    val unroll = range.u
    override def emit(): String =
      s"""$name [shape=invhouse, label="$i=$start..$end unroll $unroll"]"""
  }

  class Table(width: Int, rev: Boolean = false) extends Emittable {

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
      val rows = (if (rev) table else table.reverse).map { row =>
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
      t.addFullRow(("id", s"$name:$typ"))
      value.map { s => t.addFullRow(("value", s)) }
      t.emit
    }

    def mkArrTable(name: String, typ: String, nbanks: Int) = {
      val t = new Table(nbanks)
      t.addFullRow(("id", s"$name:$typ"))
      t.addRow((0 until nbanks).map { i => (s"bank$i", s"$i") }.toList)
      t.emit
    }

    def mkExprTable(expr: String) = {
      val t = new Table(1)
      t.addFullRow(("expr", s"$expr"))
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

  def preamble(file: String) = {
    s"""
digraph "$file" {
node [shape=plain];
graph [dpi=400];
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

  def emitExpr(expr: Expr, nodeName: String, lhs: Boolean = false): String = {
    def exprNode(expr: Expr, nodeName: String, lhs: Boolean): String = expr match {
      case EInt(_, _) | EFloat(_) | EBool(_) => ""
      case EBinop(_, e1, e2) => exprNode(e1, nodeName, lhs) concat exprNode(e2, nodeName, lhs)
      case EArrAccess(id, idxs) => {
        val dims = id.typ match {
          case Some(TArray(_, dims)) =>
            dims.foldLeft(0)((acc, e) => acc + e._1)
          case _ => 0
        }
        val name = id.toString()
        val accessEdges = if (lhs) {
          (0 until dims).foldLeft(""){
            (acc, i) => acc concat Dot.DirectedEdgeNode(nodeName, s"$name:bank$i").emit
          }
        } else {
          (0 until dims).foldLeft(""){
            (acc, i) => acc concat Dot.DirectedEdgeNode(s"$name:bank$i", nodeName).emit
          }
        }
        val indexEdges = idxs.foldLeft("")((acc, e) => acc concat exprNode(e, nodeName, false))
        s"$accessEdges\n$indexEdges"
      }
      case ERecAccess(_, _) => ""
      case ERecLiteral(_) => ""
      case EApp(_, _) => ""
      case EVar(x) =>
        if (lhs)
          Dot.DirectedEdgeNode(nodeName, x.toString()).emit
        else
          Dot.DirectedEdgeNode(x.toString(), nodeName).emit
    }
    val table = Dot.Table.mkExprTable(s"$expr")
    Dot.PropNode(nodeName, List(Dot.Label(table))).emit() concat exprNode(expr, nodeName, lhs)
  }

  private var exprIdx = 0
  def newExprName() = {
    val s = s"expr$exprIdx"
    exprIdx += 1
    s
  }

  // private var rangeIdx = 0
  // def newRangeName() = {
  //   val s = s"range$rangeIdx"
  //   rangeIdx += 1
  //   s
  // }

  def emitCmd(cmd: Command): String = cmd match {
    case CPar(c1, c2) => emitCmd(c1) concat emitCmd(c2)
    case CSeq(_, _) => ""
    case CLet(id, _, e) => {
      // val typString = e.typ match {
      //   case None => "no type"
      //   case Some(t) => t.toString()
      // }
      // val (exprSt, node) = emitExpr(e, id.toString()) match {
      //   case (s, None) => (s, "")
      //   case (s, Some(node)) => (s, node)
      // }
      emitExpr(e, id.toString())
      // val table = Dot.Table.mkVarTable(
      //   id.toString(),
      //   typString,
      //   Some(exprSt))
      // Dot.PropNode(id.toString(), List(Dot.Label(table))).emit() concat node
    }
    case CView(_, _) => ""
    case CIf(_, _, _) => ""
    case CFor(range, par, _) => { // what to do if $i shows up multiple times
      Dot.RangeNode(range.iter.toString(), range).emit concat "\n" concat
      emitCmd(par)
    }
    case CWhile(_, _) => ""
    case CUpdate(lhs, rhs) => {
      val lhsName = newExprName()
      val rhsName = newExprName()
      val node1 = emitExpr(lhs, lhsName, true)
      val node2 = emitExpr(rhs, rhsName)
      val conn = Dot.DirectedEdgeNode(rhsName, lhsName).emit
      s"$conn\n$node1\n$node2"
    }
    case CReduce(_, _, _) => ""
    case CExpr(e) => emitExpr(e, newExprName())
    case CEmpty => ""
  }

  def emit(p: Prog, c: Config): String = {
    preamble(c.srcFile.toString()) concat
    p.defs.foldLeft("")((str, defn) => str concat emitDefn(defn)) concat
    p.decls.foldLeft("")((str, decl) => str concat emitDecl(decl)) concat
    emitCmd(p.cmd) concat
    "\n" concat
    postamble
  }
}

case object VizBackend extends Backend {
  def emitProg(p: Prog, c: Config) = Vizualization.emit(p, c)
}
