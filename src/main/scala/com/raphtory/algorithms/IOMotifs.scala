package com.raphtory.algorithms

import com.raphtory.core.model.algorithm.{GraphAlgorithm, GraphPerspective, Row}

class IOMotifs(path:String) extends GraphAlgorithm{

  override def algorithm(graph: GraphPerspective): Unit = {
    graph.select({
      vertex =>
        val edges = (vertex.explodeInEdges()
          .map(e => (e.src(), e.getTimestamp(),true)) ++
          vertex.explodeOutEdges()
          .map(e => (e.dst(),e.getTimestamp(),false)))
            .sortBy(e => e._2)
        val noMotifs = edges.size - 1
        if (noMotifs <1) {
          Row(vertex.getPropertyOrElse("name",vertex.ID()),0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
        } else {
          val motifs = edges.sliding(2)
            .map(edges => extractMotif(edges))
            .toList
            .groupBy(identity)
            .mapValues(_.size.toDouble)
          val motifArray = (0 to 7).toList.map(
            x => if (noMotifs > 0) motifs.getOrElse(x, 0.0) / noMotifs else 0.0
          ).toArray
          Row(vertex.getPropertyOrElse("name", vertex.ID()), motifArray(0),motifArray(1),motifArray(2),motifArray(3),
            motifArray(4),motifArray(5),motifArray(6),motifArray(7))
        }
    })
      .writeTo(path)
  }

  def extractMotif(edges:List[(Long,Long,Boolean)]): Int = {
    val signature = (if (edges(0)._1==edges(1)._1) 1 else 0,if (edges(0)._3) 1 else 0, if (edges(1)._3) 1 else 0 )
    val motifType = signature match {
      case (0,0,0) => 0 // message two diff people
      case (0,0,1) => 1 // send msg, receive msg from diff person
      case (0,1,0) => 2 // receive msg, send message to diff person
      case (0,1,1) => 3 // receive msg from 2 diff people
      case (1,0,0) => 4 // message same person twice
      case (1,0,1) => 5 // ping pong same person
      case (1,1,0) => 6 // ping pong person
      case (1,1,1) => 7 // get messaged by same person twice
      case _ => -1
    }
    motifType
  }
}


object IOMotifs {
  def apply(path:String) = new IOMotifs(path)
}