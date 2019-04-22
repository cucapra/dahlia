package fuselang.backend

object VizGraph {

  trait Format[T] {
    def formatNode(g: T): String
    def formatEdge(s: String, t: String): String
    def formatCluster(name: String, nodes: Set[String], subCluster: String): String
  }

  case class Cluster(nodes: Set[String], children: Set[Cluster]) {
    def ++(cl: Cluster) = {
      Cluster(nodes ++ cl.nodes, children ++ cl.children)
    }
  }

  case class Graph[T](
    nodes: Map[String, T],
    edges: Map[String, Set[String]],
    clusters: Set[Cluster]) {
    def addNode(id: String, node: T) =
      Graph(nodes + (id -> node), edges, clusters)

    def addEdge(s: String, t: String) = {
      edges get s match {
        case None => Graph(nodes, edges + (s -> Set(t)), clusters)
        case Some(ed) => Graph(nodes, edges + (s -> (ed + t)), clusters)
      }
    }

    def newCluster(cl: Cluster) = {
      Graph(nodes, edges, Set(cl))
    }

    def nodeSet() = {
      nodes.keySet
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
      Graph(newNodes, newEdges, clusters ++ g.clusters)
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
      Graph(newG.nodes, edges, clusters)
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

      Console.err.println(s"cl: ${clusters.zipWithIndex}")
      def fmtCl(childs: Set[Cluster]): String = {
        childs.zipWithIndex.foldLeft("") {
          (acc, e) =>
          val (cl, idx) = e
          acc + f.formatCluster(s"cl$idx", cl.nodes, fmtCl(cl.children))
        }
      }

      val clusterStr = fmtCl(clusters)
      s"$nodeStr\n$edgeStr\n$clusterStr"
    }
  }

  object Graph {
    def empty[T]: Graph[T] = Graph(Map.empty, Map.empty, Set.empty)
    def singleton[T](id: String, node: T) = Graph(Map(id -> node), Map.empty, Set.empty)
  }
}
