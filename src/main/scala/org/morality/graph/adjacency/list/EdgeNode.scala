package org.morality.graph.adjacency.list

class EdgeNode[T](value: T, y: Int, weight: Option[Int]) {


  override def toString(): String = {
    s"$y:${value.toString}"
  }

}
