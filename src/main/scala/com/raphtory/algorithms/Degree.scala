package com.raphtory.algorithms

import com.raphtory.core.model.algorithm.{GraphAlgorithm, GraphPerspective, Row}

/**
Description
  The degree of a node in an undirected networks counts the number of neighbours that
  node has. Its weighted degree counts the number of interactions each node has. Finally
  the edge weight counts the number of interactions which have happened across an edge.

Parameters
  path (String) : The path where the output will be written

Returns
  ID (Long) : Vertex ID
  indegree (Long) : The indegree of the node
  outdegree (Long) : The outdegree of the node
  total degree (Long) : The total degree
**/
class Degree(path:String) extends GraphAlgorithm{
  override def algorithm(graph: GraphPerspective): Unit = {
    graph.select({
      vertex =>
      val inDegree = vertex.getInNeighbours().size
      val outDegree = vertex.getOutNeighbours().size
      val totalDegree = vertex.getAllNeighbours().size
    Row(vertex.getPropertyOrElse("name", vertex.ID()), inDegree, outDegree, totalDegree)
    })
      .writeTo(path)
  }
}

object Degree{
  def apply(path:String) = new Degree(path)
}