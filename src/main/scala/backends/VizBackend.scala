package fuselang.backend

import fuselang.common._
import Syntax._
import Configuration._

private object Vizualization {

  import VizGraph._

  def preamble(file: String) = {
    // splines=ortho
    s"""
digraph "$file" {
node [shape=plain];
graph [dpi=400, rankdir=LR, newrank=true];
edge [arrowsize=0.4];
"""
  }

  def postamble = "}"

  def emitDefn(defn: Definition): Graph[Definition] = defn match {
    case FuncDef(_, _, _) => Graph.empty
    case RecordDef(_, _) => Graph.empty
  }

  def emitDecl(decl: Decl): Graph[Decl] = {
    Graph.singleton(decl.id.toString(), decl)
  }

  def emitExpr(expr: Expr, nodeName: String,
    ctx: Map[Id, String], lhs: Boolean = false): Graph[Expr] = expr match {
    case EInt(_, _) | EFloat(_) | EBool(_) => Graph.empty
    case EBinop(_, e1, e2) =>
      emitExpr(e1, nodeName, ctx, lhs) flatMerge emitExpr(e2, nodeName, ctx, lhs)
    case EArrAccess(id, idxs) => {
      // get the dimensions of the array
      val g = Graph.empty[Expr]
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
        acc flatMerge emitExpr(e, nodeName, ctx, false))
    }
    case EArrLiteral(_) => Graph.empty
    case ECast(_, _) => Graph.empty
    case ERecAccess(_, _) => Graph.empty
    case ERecLiteral(_) => Graph.empty
    case EApp(fname, args) => {
      val g = Graph.singleton(s"$fname", expr)
      args.foldLeft(g) {
        (acc, e) => {
          acc flatMerge emitExpr(e, s"$fname", ctx)
        }
      }
    }
    case EVar(x) =>
      val pre = ctx getOrElse (x, "")
      val g = Graph.empty[Expr]
      if (lhs)
        g.addEdge(nodeName, s"$pre$x")
      else
        g.addEdge(s"$pre$x", nodeName)
  }

  private var nameIdx = 0
  def newName(name: String = "var") = {
    val s = s"$name$nameIdx"
    nameIdx += 1
    s
  }

  private type CmdTag = (String, Command)
  def emitCmd(cmd: Command, ctx: Map[Id, String]): Graph[CmdTag] = cmd match {
    case CPar(c1, c2) => emitCmd(c1, ctx) flatMerge emitCmd(c2, ctx)
    case CSeq(c1, c2) => emitCmd(c1, ctx) crazyMerge emitCmd(c2, ctx) //TODO: make some viz diff here
    case CLet(id, _, e) => {
      //XXX: add prefix
      e match {
        case Some(e) => {
          emitExpr(e, s"$id", ctx).map[CmdTag](x => ("", CExpr(x))) flatMerge
          Graph.singleton[CmdTag](s"$id", (s"$id", cmd))
        }
        case None => Graph.empty
      }
    }
    case CView(_, _, _) => Graph.empty
    case CSplit(_, _, _) => Graph.empty
    case CIf(cond, tCmd, fCmd) => {
      val name = newName("cond")
      // val condNode = Graph.singleton[CmdTag](name, (name, CExpr(cond)))
      val condNode = emitExpr(cond, name, ctx).map[CmdTag](x => ("", CExpr(x)))
      val tBranch = (emitCmd(tCmd, ctx) flatMerge
        Graph.singleton(s"${name}T", (s"${name}T", CEmpty))).clusterify()
      val fBranch = (emitCmd(fCmd, ctx) flatMerge
        Graph.singleton(s"${name}F", (s"${name}F", CEmpty))).clusterify()
      val G = condNode subMerge tBranch subMerge fBranch
      G.addNode(name, (name, CExpr(cond)))
        .addEdge(name, s"${name}T")
        .addEdge(name, s"${name}F")
      // // Graph.empty[CmdTag].addEdge(name, t: String)
      // Graph.singleton[CmdTag](name, (name, cmd)) merge
      // tBranch merge fBranch
      // Graph.empty
    }
    case CFor(range, par, combine) => {
      val name = newName("iter")
      val parNodes = (0 until range.u).foldLeft(Graph.empty[CmdTag]) {
        (acc, _) => acc flatMerge emitCmd(par, ctx + (range.iter -> name)).clusterify()
      }
      // val parNode = emitCmd(par, ctx + (range.iter -> name))
      // val parNode2 = emitCmd(par, ctx + (range.iter -> name))
      val combineNode = emitCmd(combine, ctx + (range.iter -> name))
      (Graph.singleton[CmdTag](s"$name${range.iter}", (name, cmd)) flatMerge
        combineNode flatMerge
        parNodes).clusterify()
      // Console.err.println("dogg")
      // Console.err.println(g.cluster)
    }
    case CWhile(_, _) => Graph.empty
    case CUpdate(lhs, rhs) => {
      val name = newName("expr")
      val lhsName = s"lhs$name"
      val rhsName = s"rhs$name"
      val node1 = emitExpr(lhs, lhsName, ctx, true).map[CmdTag](x => ("", CExpr(x)))
      val node2 = emitExpr(rhs, rhsName, ctx).map[CmdTag](x => ("", CExpr(x)))
      Graph.empty[CmdTag].addEdge(rhsName, lhsName) flatMerge
      Graph.singleton[CmdTag](lhsName, (lhsName, CExpr(lhs))) flatMerge
      Graph.singleton[CmdTag](rhsName, (rhsName, CExpr(rhs))) flatMerge
      node1 flatMerge
      node2
    }
    case CReduce(_, lhs, rhs) => {
      val name = newName("reduce")
      val g = Graph.singleton[CmdTag](name, (name, cmd))
      val g2 = lhs match {
        case EVar(id) => g.addEdge(s"$id", name).addEdge(name, s"$id")
        case EArrAccess(id, _) => g.addEdge(s"$id", name).addEdge(name, s"$id")
        case _ => g
      }
      g2 flatMerge emitExpr(rhs, name, ctx).map[CmdTag](x => ("", CExpr(x)))
    }
    case CExpr(e) => emitExpr(e, newName("expr"), ctx).map[CmdTag](x => ("", CExpr(x)))
    case CEmpty => Graph.empty
  }

  def emit(p: Prog, c: Config): String = {
    val gDefs = p.defs.foldLeft(Graph.empty[Definition]) {
      (acc, defn) => acc flatMerge emitDefn(defn)
    }
    val gDecls = p.decls.foldLeft(Graph.empty[Decl]) {
      (acc, decl) => acc flatMerge emitDecl(decl)
    }
    val gCmd = emitCmd(p.cmd, Map.empty)

    preamble(s"${c.srcFile}") concat
    gDefs.format(DotFormatter.DefnFormatter) concat
    gDecls.format(DotFormatter.DeclFormatter) concat
    gCmd.format(DotFormatter.CmdFormatter) concat
    postamble
  }
}

case object VizBackend extends Backend {
  def emitProg(p: Prog, c: Config) = Vizualization.emit(p, c)
  val canGenerateHeader: Boolean = false
  // val canGenerateHeader: Boolean = false
}
