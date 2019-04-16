package fuselang.backend

import fuselang.Syntax._
// import fuselang.Errors._
import fuselang.Utils._

private object Graph {

  case class DirGraph[T](nodes: Map[String, T], edges: Map[String, Set[String]]) {
    def addNode(id: String, node: T) =
      DirGraph(nodes + (id -> node), edges)

    def addEdge(s: String, t: String) = {
      edges get s match {
        case None => DirGraph(nodes, edges + (s -> Set(t)))
        case Some(ed) => DirGraph(nodes, edges + (s -> (ed + t)))
      }
    }

    def merge(g: DirGraph[T]) = {
      val newNodes = g.nodes.foldLeft(nodes) {
        (acc, e) =>
        val (id, node) = e
        acc + (id -> node)
      }
      val newEdges = g.edges.foldLeft(edges) {
        (acc, e) =>
        val (source, tars) = e
        edges get source match {
          case None => acc + (source -> tars)
          case Some(ed) => acc + (source -> (tars ++ ed))
        }
      }
      DirGraph(newNodes, newEdges)
    }

    def foldNodes[Acc](acc: Acc)(f: (Acc, T) => Acc) = {
      nodes.foldLeft(acc) {
        (acc, e) => f(acc, e._2)
      }
    }

    def foldEdges[Acc](acc: Acc)(f: (Acc, (String, String)) => Acc) = {
      edges.foldLeft(acc) {
        (acc, elem) =>
        val (source, tars) = elem
        tars.foldLeft(acc)((acc, e) => f(acc, (source, e)))
      }
    }
  }

  def empty[T]: DirGraph[T] = DirGraph(Map.empty, Map.empty)
  def singleton[T](id: String, node: T) = DirGraph(Map(id -> node), Map.empty)
}

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
    val end = range.e
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

  // class Prog extends Emittable {
  //   var nodes: List[Node] = List()
  //   def append(n: Node) = nodes = n :: nodes
  //   override def emit() =
  //     nodes.foldLeft("")((acc, e) => acc concat e.emit)
  // }
}
private object Vizualization {

  import Graph._

  def preamble(file: String) = {
    s"""
digraph "$file" {
node [shape=plain];
graph [dpi=400];
"""
  }

  def postamble = "}"

  def emitDefn(defn: Definition): DirGraph[Dot.Node] = defn match {
    case FuncDef(_, _, _) => Graph.empty
    case RecordDef(_, _) => Graph.empty
  }

  def emitDecl(decl: Decl): DirGraph[Dot.Node] = {
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
    val node = Dot.PropNode(decl.id.toString(), List(Dot.Label(table)))
    Graph.singleton(decl.id.toString(), node)
  }

  def emitExpr(expr: Expr, nodeName: String,
    ctx: Map[Id, String], lhs: Boolean = false): DirGraph[Dot.Node] = expr match {
    case EInt(_, _) | EFloat(_) | EBool(_) => Graph.empty
    case EBinop(_, e1, e2) =>
      emitExpr(e1, nodeName, ctx, lhs) merge emitExpr(e2, nodeName, ctx, lhs)
    case EArrAccess(id, idxs) => {
      // get the dimensions of the array
      val g = Graph.empty[Dot.Node]
      val banks = id.typ match {
        case Some(TArray(_, dims)) =>
          dims.foldLeft(0)((acc, e) => acc + e._2)
        case _ => 0
      }
      val pre = ctx getOrElse (id, "")
      val accessEdges = if (lhs) {
        (0 until banks).foldLeft(g) {
          (acc, i) => acc.addEdge(nodeName, s"$pre$id:bank$i")
        }
      } else {
        (0 until banks).foldLeft(g) {
          (acc, i) => acc.addEdge(s"$pre$id:bank$i", nodeName)
        }
      }
      idxs.foldLeft(accessEdges)((acc, e) =>
        acc merge emitExpr(e, nodeName, ctx, false))
    }
    case ERecAccess(_, _) => Graph.empty
    case ERecLiteral(_) => Graph.empty
    case EApp(_, _) => Graph.empty
    case EVar(x) =>
      val pre = ctx getOrElse (x, "")
      // if (lhs)
      // val node = Dot.PropNode(nodeName, List(Dot.Label(s"$pre$x")))
      val g = Graph.empty[Dot.Node]
      if (lhs)
        g.addEdge(nodeName, s"$pre$x")
      else
        g.addEdge(s"$pre$x", nodeName)
        // Graph.singleton(nodeName, Dot.DirectedEdgeNode(nodeName, s"$pre$x"))
      // else
        // Graph.singleton(nodeName, Dot.PropNode(nodeName, List(Dot.Label(s"$pre$x"))))
        // Graph.singleton(s"$pre$x", Dot.DirectedEdgeNode(s"$pre$x", nodeName))
  }
  // }

  private var nameIdx = 0
  def newName(name: String = "var") = {
    val s = s"$name$nameIdx"
    nameIdx += 1
    s
  }

  // private var rangeIdx = 0
  // def newRangeName() = {
  //   val s = s"range$rangeIdx"
  //   rangeIdx += 1
  //   s
  // }

  // val table = Dot.Table.mkExprTable(s"$expr")
  // Dot.PropNode(nodeName, List(Dot.Label(table))).emit() concat
  // exprNode(expr, nodeName, ctx, lhs)

  def emitCmd(cmd: Command, ctx: Map[Id, String]): DirGraph[Dot.Node] = cmd match {
    case CPar(c1, c2) => emitCmd(c1, ctx) merge emitCmd(c2, ctx)
    case CSeq(c1, c2) => emitCmd(c1, ctx) merge emitCmd(c2, ctx)
    case CLet(id, _, e) => {
      val t = new Dot.Table(2)
      t.addRow(List(("name", s"$id"), ("expr", s"$e")))
      //XXX: add prefix
      emitExpr(e, s"$id", ctx) merge
      Graph.singleton(s"$id", Dot.PropNode(s"$id", List(Dot.Label(t.emit()))))
    }
    case CView(_, _) => Graph.empty
    case CIf(_, _, _) => Graph.empty
    case CFor(range, par, combine) => { // what to do if $i shows up multiple times
      val name = newName("iter")
      val rangeNode = Dot.RangeNode(s"$name${range.iter}", range)
      val parNode = emitCmd(par, ctx + (range.iter -> name))
      val combineNode = emitCmd(combine, ctx + (range.iter -> name))
//       val subgraph = s"""subgraph cluster_${newName("subgraph")} {
// graph[style="dotted"]"""
      Graph.singleton[Dot.Node](rangeNode.name, rangeNode) merge parNode merge combineNode
    }
    case CWhile(_, _) => Graph.empty
    case CUpdate(lhs, rhs) => {
      val lhsName = newName("expr")
      val rhsName = newName("expr")
      val t1 = Dot.PropNode(lhsName, List(Dot.Label(Dot.Table.mkExprTable(s"$lhs"))))
      val t2 = Dot.PropNode(rhsName, List(Dot.Label(Dot.Table.mkExprTable(s"$rhs"))))
      val node1 = emitExpr(lhs, lhsName, ctx, true)
      val node2 = emitExpr(rhs, rhsName, ctx)
      // val conn = Dot.DirectedEdgeNode(rhsName, lhsName)
      Graph.singleton[Dot.Node](lhsName, t1) merge
      Graph.singleton[Dot.Node](rhsName, t2) merge
      node1 merge
      node2
    }
    case CReduce(op, lhs, rhs) => {
      val name = newName("reduce")
      val node = Dot.PropNode(name, List(Dot.Label(s""""$op""""), Dot.Shape("diamond")))
      val g = Graph.singleton[Dot.Node](name, node)
      val g2 = lhs match {
        case EVar(id) => g.addEdge(s"$id", name).addEdge(name, s"$id")
        case EArrAccess(id, _) => g.addEdge(s"$id", name).addEdge(name, s"$id")
        case _ => g
      }
      g2 merge emitExpr(rhs, name, ctx)
    }
    case CExpr(e) => emitExpr(e, newName("expr"), ctx)
    case CEmpty => Graph.empty
  }

  def emit(p: Prog, c: Config): String = {
    val gDefs = p.defs.foldLeft(Graph.empty[Dot.Node]) {
      (acc, defn) => acc merge emitDefn(defn)
    }
    val gDecls = p.decls.foldLeft(Graph.empty[Dot.Node]) {
      (acc, decl) => acc merge emitDecl(decl)
    }
    val gCmd = emitCmd(p.cmd, Map.empty)
    val total = gDefs merge gDecls merge gCmd

    preamble(s"${c.srcFile}") concat
    total.foldNodes("") { (acc, node) => acc concat node.emit } concat
    total.foldEdges(List[String]()) {
      (acc, edge) =>
      val (source, tar) = edge
      Dot.DirectedEdgeNode(source, tar).emit :: acc
    }.mkString("\n") concat
    "\n" concat
    postamble
    // p.decls.foldLeft("")((str, decl) => str concat emitDecl(decl)) merge
    // emitCmd(p.cmd, Map.empty)

    // preamble(c.srcFile.toString()) concat
    // p.defs.foldLeft("")((str, defn) => str concat emitDefn(defn)) concat
    // p.decls.foldLeft("")((str, decl) => str concat emitDecl(decl)) concat
    // emitCmd(p.cmd, Map.empty) concat
    // "\n" concat
    // postamble
  }
}
// private object Vizualization {

//   def preamble(file: String) = {
//     s"""
// digraph "$file" {
// node [shape=plain];
// graph [dpi=400];
// """
//   }

//   def postamble = "}"

//   def emitDefn(defn: Definition): String = defn match {
//     case FuncDef(_, _, _) => ""
//     case RecordDef(_, _) => ""
//   }

//   def emitDecl(decl: Decl): String = {
//     val table = decl.typ match {
//       case TArray(typ, dims) => Dot.Table.mkArrTable(
//         decl.id.toString(),
//         typ.toString(),
//         dims.foldLeft(0)((acc, e) => acc + e._2) // e._2 is banking factor
//       )
//       case typ => Dot.Table.mkVarTable(
//         decl.id.toString(),
//         typ.toString(),
//         None)
//     }
//     val p = new Dot.Prog
//     p.append(
//       Dot.PropNode(decl.id.toString(), List(Dot.Label(table))))
//     p.emit
//   }

//   def emitExpr(expr: Expr, nodeName: String,
//     ctx: Map[Id, String], lhs: Boolean = false): String = expr match {
//     case EInt(_, _) | EFloat(_) | EBool(_) => ""
//     case EBinop(_, e1, e2) =>
//       emitExpr(e1, nodeName, ctx, lhs) concat emitExpr(e2, nodeName, ctx, lhs)
//     case EArrAccess(id, idxs) => {
//       val dims = id.typ match {
//         case Some(TArray(_, dims)) =>
//           dims.foldLeft(0)((acc, e) => acc + e._2)
//         case _ => 0
//       }
//       val pre = ctx getOrElse (id, "")
//       val accessEdges = if (lhs) {
//         (0 until dims).foldLeft(""){
//           (acc, i) => acc concat Dot.DirectedEdgeNode(nodeName, s"$pre$id:bank$i").emit
//         }
//       } else {
//         (0 until dims).foldLeft(""){
//           (acc, i) => acc concat Dot.DirectedEdgeNode(s"$pre$id:bank$i", nodeName).emit
//         }
//       }
//       val indexEdges = idxs.foldLeft("")((acc, e) =>
//         acc concat emitExpr(e, nodeName, ctx, false))
//       s"$accessEdges\n$indexEdges"
//     }
//     case ERecAccess(_, _) => ""
//     case ERecLiteral(_) => ""
//     case EApp(_, _) => ""
//     case EVar(x) =>
//       val pre = ctx getOrElse (x, "")
//       if (lhs)
//         Dot.DirectedEdgeNode(nodeName, s"$pre$x").emit
//       else
//         Dot.DirectedEdgeNode(s"$pre$x", nodeName).emit
//   }
//   // }

//   private var nameIdx = 0
//   def newName(name: String = "var") = {
//     val s = s"$name$nameIdx"
//     nameIdx += 1
//     s
//   }

//   // private var rangeIdx = 0
//   // def newRangeName() = {
//   //   val s = s"range$rangeIdx"
//   //   rangeIdx += 1
//   //   s
//   // }

//   // val table = Dot.Table.mkExprTable(s"$expr")
//   // Dot.PropNode(nodeName, List(Dot.Label(table))).emit() concat
//   // exprNode(expr, nodeName, ctx, lhs)

//   def emitCmd(cmd: Command, ctx: Map[Id, String]): String = cmd match {
//     case CPar(c1, c2) => List(emitCmd(c1, ctx), emitCmd(c2, ctx)).mkString("\n")
//     case CSeq(c1, c2) => List(emitCmd(c1, ctx), emitCmd(c2, ctx)).mkString("\n")
//     case CLet(id, _, e) => {
//       val t = new Dot.Table(2)
//       t.addRow(List(("name", s"$id"), ("expr", s"$e")))
//       Dot.PropNode(s"$id", List(Dot.Label(t.emit()))).emit concat emitExpr(e, s"$id", ctx)
//     }
//     case CView(_, _) => ""
//     case CIf(_, _, _) => ""
//     case CFor(range, par, combine) => { // what to do if $i shows up multiple times
//       val name = newName("iter")
//       val rangeNode = Dot.RangeNode(s"$name${range.iter}", range).emit
//       val parNode = emitCmd(par, ctx + (range.iter -> name))
//       val combineNode = emitCmd(combine, ctx + (range.iter -> name))
//       val subgraph = s"""subgraph cluster_${newName("subgraph")} {
// graph[style="dotted"]"""
//       List(rangeNode, subgraph, parNode, "}", combineNode).mkString("\n")
//     }
//     case CWhile(_, _) => ""
//     case CUpdate(lhs, rhs) => {
//       val lhsName = newName("expr")
//       val rhsName = newName("expr")
//       val t1 = Dot.PropNode(lhsName, List(Dot.Label(Dot.Table.mkExprTable(s"$lhs")))).emit()
//       val t2 = Dot.PropNode(rhsName, List(Dot.Label(Dot.Table.mkExprTable(s"$rhs")))).emit()
//       val node1 = emitExpr(lhs, lhsName, ctx, true)
//       val node2 = emitExpr(rhs, rhsName, ctx)
//       val conn = Dot.DirectedEdgeNode(rhsName, lhsName).emit
//       List(conn, t1, t2, node1, node2).mkString("\n")
//     }
//     case CReduce(op: ROp, lhs: Expr, rhs: Expr) => {
//       val name = newName("expr")
//       val opNode = Dot.PropNode(name, List(Dot.Label(s""""$op""""), Dot.Shape("diamond"))).emit
//       val lEdge = lhs match {
//         case EVar(id) => Dot.TwoEdgeNode(id.toString(), name).emit
//         case EArrAccess(id, _) => Dot.TwoEdgeNode(id.toString(), name).emit;
//         case _ => ""
//       }
//       val rEdge = emitExpr(rhs, name, ctx)
//       s"$rEdge\n$lEdge\n$opNode"
//     }
//     case CExpr(e) => emitExpr(e, newName("expr"), ctx)
//     case CEmpty => ""
//   }

//   def emit(p: Prog, c: Config): String = {
//     preamble(c.srcFile.toString()) concat
//     p.defs.foldLeft("")((str, defn) => str concat emitDefn(defn)) concat
//     p.decls.foldLeft("")((str, decl) => str concat emitDecl(decl)) concat
//     emitCmd(p.cmd, Map.empty) concat
//     "\n" concat
//     postamble
//   }
// }

case object VizBackend extends Backend {
  def emitProg(p: Prog, c: Config) = Vizualization.emit(p, c)
}
