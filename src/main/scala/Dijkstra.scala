import guru.nidi.graphviz.model.{Link, MutableGraph, MutableNode}
import guru.nidi.graphviz.parse.Parser
import better.files.File

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Dijkstra extends App {

  val dotFileContents: String = File("E:\\Dev\\Repos\\graph\\src\\test\\resources\\digraph1.dot").contentAsString
  val graph: MutableGraph = new Parser().read(dotFileContents)

  def nodeWithName(name : String) : Node =
    nodes.find(_.name == name).get

  def path(from : Node) : List[Node] =
    from.shortestPreviousNode match {
      case None => from :: Nil
      case Some(previousNode) => from :: path(previousNode)
    }

  val nodes: Seq[Node] = graph.nodes().asScala.map(Node(_, "a")).toSeq
  val startNode: Node = nodeWithName("a")
  val endNode: Node = nodeWithName("b")
  val priorityQueue: mutable.PriorityQueue[Node] = mutable.PriorityQueue.from(nodes).reverse

  def dijkstra(): Option[Seq[Node]] = {
    while (priorityQueue.nonEmpty) {
      val currentNode: Node = priorityQueue.dequeue()
      if (currentNode == endNode) {
        return Some(path(currentNode).reverse)
      } else {
        currentNode.edges.foreach(edge => {
          val newDistance = currentNode.distance + edge.distance
          if (newDistance < edge.to(nodes).distance) {
            edge.to(nodes).distance = newDistance
            edge.to(nodes).shortestPreviousNode = Some(currentNode)
          }
        })
        // Make sure all elements are ordered again.
        priorityQueue.enqueue(priorityQueue.dequeueAll:_*)
      }
    }
    None
  }

  println(dijkstra().get.map(_.name).mkString)
}

case class Node(n : MutableNode, var distance : Float, var shortestPreviousNode : Option[Node] = None) extends Ordered[Node] {
  def name : String = n.name().toString
  def edges : Seq[Edge] = n.links().asScala.map(Edge).toSeq
  override def compare(that: Node): Int = this.distance.compareTo(that.distance)

  override def toString: String = s"Node(name=$name, distance=$distance, edges=${edges.map(_.toString).mkString(",")}, shortestPreviousNodes=$shortestPreviousNode)"
}

case class Edge(l : Link) {
  def distance : Float = l.attrs().get("weight").toString.toFloat
  def to(allNodes : Seq[Node]) : Node = allNodes.find(_.name == l.to().name().toString).get

  override def toString: String =
    s"Link(to=${this.l.to().name().toString},distance=${this.distance})"
}

object Node {
  def apply(n : MutableNode, startNodeName : String) : Node = {
    Node(n, if (n.name().toString == startNodeName) 0 else Float.MaxValue)
  }
}

