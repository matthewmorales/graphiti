package org.morality.graph.adjacency.list

class EdgeNode(val y: Int, val weight: Option[Int], val next: Option[EdgeNode] = None) {


  override def toString(): String = {
    s"$y"
  }

}
