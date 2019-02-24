package org.morality.graph.adjacency.list

import org.scalatest.FlatSpec

import scala.io.Source

class AdjacencyListGraphSpec extends FlatSpec {

  def readGraphFromFile(fileName: String): AdjacencyListGraph = {
    val bufferedSource = Source.fromResource(fileName)
    val wroteSource: Seq[Array[Int]] = bufferedSource.getLines().map { line =>
      line.split('|').map(_.toInt)
    }.toSeq

    val firstLine = wroteSource.head
    val edgeLines = wroteSource.tail

    val nVertices = firstLine(0)
    val nEdges = edgeLines.size

    var graph = AdjacencyListGraph[String](nVertices, nEdges, directed = false)
    var counter = 0
    edgeLines.foreach { line =>
      graph = graph.insertEdge(line(0), line(1), None, directedInsert = false)
      counter = counter + 1
    }
    bufferedSource.close


    graph
  }


  "printGraph" should "printGraph and terminate" in {
    val graph = readGraphFromFile("test_graph_1.txt")
    graph.printGraph()
    assert(true)
  }

  "bfs" should "shouldn't fail" in {
    val graph = readGraphFromFile("test_graph_1.txt")
    graph.bfs()
    assert(true)
  }

}
