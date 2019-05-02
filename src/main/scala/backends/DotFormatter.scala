package fuselang.backend

import fuselang.Syntax._
// import fuselang.Utils._

private object Dot {

  type Name = String

  sealed trait Emittable {
    def emit(): String = ""
  }

  sealed trait Prop extends Emittable
  case class Label(str: String) extends Prop {
    override def emit(): String = s"label=$str"
  }
  case class Shape(str: String) extends Prop {
    override def emit(): String = s"shape=$str"
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
  case class TwoEdgeNode(src: Name, tar: Name) extends Node {
    override def emit(): String = s"$src -> $tar;\n$tar -> $src;"
  }
  case class RangeNode(name: String, range: CRange) extends Node {
    val i = range.iter
    val start = range.s
    val end = range.e / range.u
    override def emit(): String =
      s"""$name [shape=invhouse, label="$i=$start..$end"]"""
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

    private def escape(s: String) = {
      val map = Map(
        "&" -> "&amp;",
        "<" -> "&lt;",
        ">" -> "&gt;")
      map.foldLeft(s)((acc, e) => {
        val (ch, rp) = e
        acc.replace(ch, rp)
      })
    }

    override def emit(): String = {
      val rows = (if (rev) table else table.reverse).map { row =>
        val s = row.map {
          case Item(name, value, w) =>
            s"""<td port='$name' colspan='$w'>${escape(value)}</td>"""
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
}

object DotFormatter {

  import VizGraph._

  trait DefaultFormatter {
    def formatEdge(s: String, t: String): String = Dot.DirectedEdgeNode(s, t).emit
    def formatCluster(name: String, nodes: Set[String], subCluster: String): String = {
      val nodeStr = nodes.foldLeft("")((acc, e) => s"$e; $acc")
      s"""
subgraph cluster_$name {
graph [style="dashed"];
$nodeStr
$subCluster
}
"""
    }
  }

  object DefnFormatter extends Format[Definition] with DefaultFormatter {
    def formatNode(defn: Definition) = ""
  }

  object DeclFormatter extends Format[Decl] with DefaultFormatter {
    def formatNode(decl: Decl) = {
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
      Dot.PropNode(decl.id.toString(), List(Dot.Label(table))).emit
    }
  }

  object ExprFormatter extends Format[(String, Expr)] with DefaultFormatter {
    def formatNode(p: (String, Expr)) = {
      val (name, expr) = p
      expr match {
        case EApp(fname, _) => {
          val t = new Dot.Table(1)
          t.addFullRow(("func", s"$expr"))
          Dot.PropNode(s"$fname", List(Dot.Label(t.emit))).emit
        }
        case e =>
          Dot.PropNode(s"$name", List(Dot.Label(Dot.Table.mkExprTable(s"$e")))).emit
      }
    }
  }

  object CmdFormatter extends Format[(String, Command)] with DefaultFormatter {
    def formatNode(p: (String, Command)) = {
      val (name, cmd) = p
      cmd match {
        case CLet(id, _, e) => {
          val t = new Dot.Table(2)
          t.addRow(List(("name", s"$id"), ("expr", s"$e")))
          Dot.PropNode(s"$id", List(Dot.Label(t.emit))).emit
        }
        case CIf(cond, _, _) => {
          Dot.PropNode(s"$name", List(Dot.Label(Dot.Table.mkExprTable(s"$cond")))).emit
        }
        case CFor(range, _, _) => Dot.RangeNode(s"$name${range.iter}", range).emit
        case CReduce(op, _, _) =>
          Dot.PropNode(name, List(Dot.Label(s""""$op""""), Dot.Shape("diamond"))).emit
        case CExpr(e) => ExprFormatter.formatNode((name, e))
        case _ => ""
      }
    }
  }
}
