package org.morality.graph.adjacency.list

import org.morality.graph.Graph

class AdjacencyListGraph[T] private (edges: Array[Seq[EdgeNode[T]]],
                                     degree: Array[Int], numVertices: Int,
                                     numEdges: Int, directed: Boolean
                                    ) extends Graph {

  /**
    * Inserts new edge from X to Y on to graph. If insertion is NOT directed, it will also insert edge from
    * Y to X. Returns a new instance of the graph, does not modify current graph.
    */
  def insertEdge(value: T, x: Int, y: Int, weight: Option[Int], directedInsert: Boolean = directed): AdjacencyListGraph[T] = {

    val edgeNode = new EdgeNode(value, y, weight)

    val newDegrees = degree.clone()
    val oldDegree: Int = degree(x)
    newDegrees.update(x, oldDegree + 1)

    val newEdges = edges.clone()
    val xAdjacencies: Seq[EdgeNode[T]] = newEdges(x)

    newEdges.update(x ,xAdjacencies :+ edgeNode)

    if (!directedInsert) {
      val temp = new AdjacencyListGraph[T](newEdges, newDegrees, numVertices, numEdges, directed)
      temp.insertEdge(value, y, x, weight, directedInsert = true)
    } else {
      new AdjacencyListGraph[T](newEdges, newDegrees, numVertices, numEdges + 1, directed)
    }
  }

  def printGraph(): Unit = {
    var counter = 0
    edges.foreach { adjacents =>
      print(s"$counter: ")
      adjacents.foreach { neighbor: EdgeNode[T] =>
        print(s" ${neighbor.toString()}")
      }
      println("")
      println("---------")
      counter = counter + 1
    }
  }
}

object AdjacencyListGraph {
  def apply[T](numVertices: Int,
            numEdges: Int, directed: Boolean
           ): AdjacencyListGraph[T] = {

    val emptyAdjacencies: Array[Seq[EdgeNode[T]]] = Array.fill[Seq[EdgeNode[T]]](numVertices)(Seq.empty)
    val emptyDegrees: Array[Int] = Array.fill[Int](numVertices)(0)

    new AdjacencyListGraph(emptyAdjacencies, emptyDegrees, numVertices, numEdges, directed)
  }

  def apply[T](directed: Boolean): AdjacencyListGraph[T] = {
    new AdjacencyListGraph[T](edges = Array.empty, degree = Array.empty, numVertices = 0, numEdges = 0, directed)
  }

  def initializedEmptyDirectedGraph[T](): AdjacencyListGraph[T] = apply[T](directed = true)
  def initializedEmptyUndirectedGraph[T](): AdjacencyListGraph[T] = apply[T](directed = false)
}
