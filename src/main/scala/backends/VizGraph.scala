package fuselang.backend

object VizGraph {

  trait Format[T] {
    def formatNode(g: T): String
    def formatEdge(s: String, t: String): String
  }

  case class Graph[T](nodes: Map[String, T], edges: Map[String, Set[String]]) {
    def addNode(id: String, node: T) =
      Graph(nodes + (id -> node), edges)

    def addEdge(s: String, t: String) = {
      edges get s match {
        case None => Graph(nodes, edges + (s -> Set(t)))
        case Some(ed) => Graph(nodes, edges + (s -> (ed + t)))
      }
    }

    def merge(g: Graph[T]) = {
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
      Graph(newNodes, newEdges)
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

    def map[B](f: T => B): Graph[B] = {
      val newG = nodes.foldLeft(Graph.empty[B]) {
        case (acc, (id, t)) => acc.addNode(id, f(t))
      }
      Graph(newG.nodes, edges)
    }

    def format(f: Format[T]) = {
      val nodeStr = foldNodes("") {
        (acc, t) => s"$acc${f.formatNode(t)}\n"
      }
      val edgeStr = foldEdges("") {
        (acc, t) =>
        val es = f.formatEdge(t._1, t._2)
        s"$acc$es\n"
      }
      s"$nodeStr\n$edgeStr"
    }
  }

  object Graph {
    def empty[T]: Graph[T] = Graph(Map.empty, Map.empty)
    def singleton[T](id: String, node: T) = Graph(Map(id -> node), Map.empty)
  }
}
