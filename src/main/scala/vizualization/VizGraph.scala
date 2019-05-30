package fuselang.vizualization

object VizGraph {

  sealed trait Cluster[T] {

    def +(item: T): Cluster[T] = this match {
      case ClusterLeaf(_) => ClusterTree(Set(this, ClusterLeaf(item)))
      case ClusterTree(childs) => ClusterTree(childs + ClusterLeaf(item))
    }

    /** [a ++ b] merges cluster [a] and cluster [b] together. If both
      * [a] and [b] are leaves, then [a ++ b] is a new tree with leaves
      * [a] and [b]. If [a] is a leaf and [b] is a tree, then [a ++ b]
      * is [b] with an additional leaf [a]. If both [a] and [b] are trees,
      * then [a ++ b] is the set addition of [a] and [b] (leaves in common
      * are not duplicated in the result).
      */
    def ++(that: Cluster[T]): Cluster[T] = (this, that) match {
      case (a@ClusterLeaf(_), b@ClusterLeaf(_)) => ClusterTree(Set(a, b))
      case (ClusterTree(_), ClusterLeaf(node)) => this + node
      case (ClusterLeaf(node), ClusterTree(_)) => that + node
      case (ClusterTree(childs1), ClusterTree(childs2)) => ClusterTree(childs1 ++ childs2)
    }

    def sub(cl: Cluster[T]): Cluster[T] = {
      Cluster.fromSet(this.flatten() -- cl.flatten())
    }

    def size(): Int = this.flatten().size

    def flatten(): Set[T] = this match {
      case ClusterLeaf(n) => Set(n)
      case ClusterTree(s) => s.foldLeft(Set.empty[T])((acc, e) => acc ++ e.flatten())
    }

    def isEmpty(): Boolean = this match {
      case ClusterLeaf(_) => false
      case ClusterTree(s) => s.isEmpty
    }

    def prune(): Cluster[T] = this match {
      case ClusterLeaf(_) => this
      case ClusterTree(childs) => ClusterTree(childs.filter ( ch => ch match {
        case ClusterLeaf(_) => true
        case ClusterTree(s) => s.nonEmpty
      }))
    }

    def clean(): Cluster[T] = this match {
      case ClusterLeaf(_) => this
      case ClusterTree(childs) => {
        if (childs.size == 1) {
          childs.head.clean()
        } else {
          this
        }
      }
    }

    // def +(t: T): Cluster[T] = this ++ ClusterLeaf(t)

    def subCluster(cl: Cluster[T]): Cluster[T] = this match {
      case node@ClusterLeaf(_) => node ++ ClusterTree(Set(cl))
      case cs@ClusterTree(set) =>
        if (set.isEmpty) cl
        else cl match {
          case a@ClusterLeaf(_) => ClusterTree(Set(cs, a))
          case as@ClusterTree(set) => if (set.isEmpty) cs else ClusterTree(Set(cs, as))
        }
    }
  }
  case class ClusterLeaf[T](node: T) extends Cluster[T]
  case class ClusterTree[T](children: Set[Cluster[T]]) extends Cluster[T]
  object Cluster {
    def empty[T]: Cluster[T] = ClusterTree(Set())
    def fromSet[T](s: Set[T]): Cluster[T] = {
      ClusterTree(s.map((x) => ClusterLeaf(x)))
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
      val nCluster = if (cluster.isEmpty()) {
        Cluster.fromSet(nodes.keySet)
      } else {
        (Cluster.fromSet(nodes.keySet) sub cluster) ++ ClusterTree(Set(cluster))
      }
      Console.err.println("here: " + nodes)
      Console.err.println(s"$cluster -> $nCluster")
      Graph(nodes, edges, nCluster)
      // val diff = Cluster.fromSet(nodes.keySet) sub cluster
      // if (diff.size() == 0) {
      //   Graph(nodes, edges, cluster)
      // } else {
      //   Graph(nodes, edges, diff ++ ClusterTree(Set(cluster)))
      // }
    }

    private def primMerge(g: Graph[T]) = {
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
      (newNodes, newEdges)
    }

    def flatMerge(g: Graph[T]) = {
      val (newNodes, newEdges) = this.primMerge(g)
      Graph(newNodes, newEdges, cluster ++ g.cluster)
    }

    def clustMerge(g: Graph[T]) = {
      val (newNodes, newEdges) = this.primMerge(g)
      Graph(newNodes, newEdges, ClusterTree(Set(cluster, g.cluster)).prune().clean())
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
        case ClusterLeaf(a) => a
        case ClusterTree(as) =>
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

      val clusterStr = cluster match {
        case ClusterLeaf(_) => ""
        case ClusterTree(as) =>
          as.foldLeft((0, ""))((acc, e) =>
            (acc._1+1, s"${fmtCl(e, acc._1+1)}; ${acc._2}"))._2
      }
      // val clusterStr = fmtCl(cluster, 0)
      Console.err.println(cluster)
      // Console.err.println(clusterStr)
      s"$nodeStr\n$edgeStr\n$clusterStr"
    }
  }

  object Graph {
    def empty[T]: Graph[T] = Graph(Map.empty, Map.empty, Cluster.empty)
    def singleton[T](id: String, node: T) = Graph(Map(id -> node), Map.empty, Cluster.empty)
  }
}
