package org.morality.graph.adjacency.list

import org.morality.graph.Graph

import scala.collection.mutable

class AdjacencyListGraph private (edges: Array[Option[EdgeNode]],
                                     degree: Array[Int], numVertices: Int,
                                     numEdges: Int, directed: Boolean
                                    ) extends Graph {

  def bfs(): Unit = {
    val queue = new mutable.Queue[Int]()
    val processed: Array[Boolean] = Array.fill(numVertices)(false)
    val discovered: Array[Boolean] = Array.fill(numVertices)(false)
    val parent: Array[Int] = Array.fill(numVertices)(-1)
    val start = 0

    queue.enqueue(start)
    discovered.update(start, true)


    while (queue.nonEmpty) {
      val currNode = queue.dequeue()

      processVertexEarly(() => println(s"processed $currNode early"))

      processed.update(currNode, true)
      var edgeToVisit: Option[EdgeNode] = edges(currNode)
      while (edgeToVisit.nonEmpty) {
        val y = edgeToVisit.get.y
        if (!processed(y) || directed) {
          processEdge( () => println(s"proccessed edge $currNode -> $y"))
        }
        if (!discovered(y)) {
          queue.enqueue(y)
          discovered.update(y, true)
          parent.update(y, currNode)
        }
        edgeToVisit = edgeToVisit.get.next
      }


      processVertexLate(() => println(s"processed $currNode late"))
    }

  }

  private def processVertexLate(processFunction: () => Any): Unit = {
    processFunction()
  }

  private def processVertexEarly(processFunction: () => Any): Unit = {
    processFunction()
  }

  private def processEdge(processFunction: () => Any): Unit = {
    processFunction()
  }



  /**
    * Inserts new edge from X to Y on to graph. If insertion is NOT directed, it will also insert edge from
    * Y to X. Returns a new instance of the graph, does not modify current graph.
    */
  def insertEdge(x: Int, y: Int, weight: Option[Int], directedInsert: Boolean = directed): AdjacencyListGraph = {

    val edgeNode = new EdgeNode(y, weight, edges(x))

    val newDegrees = degree.clone()
    val oldDegree: Int = degree(x)
    newDegrees.update(x, oldDegree + 1)

    val newEdges = edges.clone()

    newEdges.update(x ,Some(edgeNode))

    if (!directedInsert) {
      val temp = new AdjacencyListGraph(newEdges, newDegrees, numVertices, numEdges, directed)
      temp.insertEdge(y, x, weight, directedInsert = true)
    } else {
      new AdjacencyListGraph(newEdges, newDegrees, numVertices, numEdges + 1, directed)
    }
  }

  def printGraph(): Unit = {

    for ( count <- edges.indices) {
      print(s"$count degree: ${degree(count)} | adjacent nodes: ")
      var next: Option[EdgeNode] = edges(count)
      while (next.nonEmpty) {
        print(s"${next.get.toString()}")
        next = next.get.next
      }
      println("")
      println("---------")
    }
  }
}

object AdjacencyListGraph {
  def apply[T](numVertices: Int,
            numEdges: Int, directed: Boolean
           ): AdjacencyListGraph = {

    val emptyAdjacencies: Array[Option[EdgeNode]] = Array.fill[Option[EdgeNode]](numVertices)(None)
    val emptyDegrees: Array[Int] = Array.fill[Int](numVertices)(0)

    new AdjacencyListGraph(emptyAdjacencies, emptyDegrees, numVertices, numEdges, directed)
  }

  def apply(directed: Boolean): AdjacencyListGraph = {
    new AdjacencyListGraph(edges = Array.empty, degree = Array.empty, numVertices = 0, numEdges = 0, directed)
  }

  def initializedEmptyDirectedGraph[T](): AdjacencyListGraph = apply(directed = true)
  def initializedEmptyUndirectedGraph[T](): AdjacencyListGraph = apply(directed = false)
}
