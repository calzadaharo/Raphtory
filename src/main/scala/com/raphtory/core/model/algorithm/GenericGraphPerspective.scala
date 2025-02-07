package com.raphtory.core.model.algorithm

import com.raphtory.core.model.graph.visitor.Vertex

import scala.collection.mutable

class GenericGraphPerspective(vertices:Int)  extends GraphPerspective{
  val graphOpps = mutable.Queue[GraphFunction]()
  val table     = new GenericTable()

  override def filter(f: Vertex => Boolean): GraphPerspective = {
    graphOpps.enqueue(VertexFilter(f))
    this
  }

  override def step(f: Vertex => Unit): GraphPerspective = {
    graphOpps.enqueue(Step(f))
    this
  }

  override def iterate(f: Vertex => Unit, iterations: Int,executeMessagedOnly:Boolean): GraphPerspective = {
    graphOpps.enqueue(Iterate(f,iterations,executeMessagedOnly))
    this
  }

  override def select(f: Vertex => Row): Table = {
    graphOpps.enqueue(Select(f))
    table
  }

  def getNextOperation():Option[GraphFunction] = if (graphOpps.nonEmpty) Some(graphOpps.dequeue()) else None

  def getTable() = table

  override def nodeCount(): Int = vertices

}
