package fuselang.vizualization

object VizGraph {

  trait Cluster[T] {
    this: Cluster[T] =>
    def ++(cl: Cluster[T]): Cluster[T] = this match {
      case a@ClusterNode(_) => cl match {
        case b@ClusterNode(_) => ClusterSet(Set(a, b))
        case ClusterSet(bs) => ClusterSet(bs + a)
      }
      case ClusterSet(as) => cl match {
        case b@ClusterNode(_) => ClusterSet(as + b)
        case ClusterSet(bs) => ClusterSet(as ++ bs)
      }
    }

    def **(cl: Cluster[T]): Cluster[T] = this match {
      case ClusterNode(_) => this ++ cl
      case as@ClusterSet(set) => if (set.isEmpty) cl else ClusterSet(Set(as, cl))
    }

    def sub(cl: Cluster[T]): Cluster[T] = {
      Cluster.fromSet(this.flatten() -- cl.flatten())
    }

    def size(): Int = this.flatten().size

    def flatten(): Set[T] = this match {
      case ClusterNode(n) => Set(n)
      case ClusterSet(s) => s.foldLeft(Set.empty[T])((acc, e) => acc ++ e.flatten())
    }

    def +(t: T): Cluster[T] = this ++ ClusterNode(t)

    def subCluster(cl: Cluster[T]): Cluster[T] = this match {
      case node@ClusterNode(_) => node ++ ClusterSet(Set(cl))
      case cs@ClusterSet(set) =>
        if (set.isEmpty) cl
        else cl match {
          case a@ClusterNode(_) => ClusterSet(Set(cs, a))
          case as@ClusterSet(set) => if (set.isEmpty) cs else ClusterSet(Set(cs, as))
        }
    }
  }
  case class ClusterNode[T](node: T) extends Cluster[T]
  case class ClusterSet[T](children: Set[Cluster[T]]) extends Cluster[T]
  object Cluster {
    def empty[T]: Cluster[T] = ClusterSet(Set())
    def fromSet[T](s: Set[T]): Cluster[T] = {
      ClusterSet(s.map((x) => ClusterNode(x)))
    }
  }

  trait Format[T] {
    def formatNode(g: T): String
    def formatEdge(s: String, t: String): String
    def formatCluster(name: String, nodes: Set[String], subCluster: String): String
  }

  case class Graph[T](
    nodes: Map[String, T],
    edges: Map[String, Set[String]],
    cluster: Cluster[String]) {
    def addNode(id: String, node: T) =
      Graph(nodes + (id -> node), edges, cluster)

    def addEdge(s: String, t: String) = {
      edges get s match {
        case None => Graph(nodes, edges + (s -> Set(t)), cluster)
        case Some(ed) => Graph(nodes, edges + (s -> (ed + t)), cluster)
      }
    }

    def clusterify() = {
      val diff = Cluster.fromSet(nodes.keySet) sub cluster
      // Console.err.println("test")
      // Console.err.println(diff)
      if (diff.size() == 0) {
        Graph(nodes, edges, cluster)
      } else {
        Graph(nodes, edges, diff ++ ClusterSet(Set(cluster)))
      }
    }

    def subMerge(sg: Graph[T]) = {
      val newG = Graph(nodes, edges, cluster) flatMerge sg
      Graph(newG.nodes, newG.edges, cluster.subCluster(Cluster.fromSet(sg.nodes.keySet)))
      // Console.err.println(s"$cluster + ${sg.cluster} =\n\t ${res.cluster}")
    }

    def crazyMerge(cg: Graph[T]) = {
      val newG = Graph(nodes, edges, cluster) flatMerge cg
      Graph(newG.nodes, newG.edges, cluster ** cg.cluster)
    }

    def flatMerge(g: Graph[T]) = {
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
      Graph(newNodes, newEdges, cluster ++ g.cluster)
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
      Graph(newG.nodes, edges, cluster)
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

      def fmtCl(cl: Cluster[String], i: Int): String = cl match {
        case ClusterNode(a) => a
        case ClusterSet(as) =>
          if (as.isEmpty)
            ""
          else {
            s"\nsubgraph cluster_cl$i {" +
            as.foldLeft((i, ""))((acc, e) =>
              if (fmtCl(e, acc._1+1) == "")
                (acc._1+1, s"${acc._2}")
               else
                 (acc._1+1, s"${fmtCl(e, acc._1+1)}; ${acc._2}"))._2 +
            "}\n"
          }
      }

      // val clusterStr = cluster match {
      //   case ClusterNode(_) => ""
      //   case ClusterSet(as) =>
      //     as.foldLeft((0, ""))((acc, e) =>
      //       (acc._1+1, s"${fmtCl(e, acc._1+1)}; ${acc._2}"))._2
      // }
      val clusterStr = fmtCl(cluster, 0)
      Console.err.println(cluster)
      Console.err.println(clusterStr)
      s"$nodeStr\n$edgeStr\n$clusterStr"
    }
  }

  object Graph {
    def empty[T]: Graph[T] = Graph(Map.empty, Map.empty, Cluster.empty)
    def singleton[T](id: String, node: T) = Graph(Map(id -> node), Map.empty, Cluster.empty)
  }
}
