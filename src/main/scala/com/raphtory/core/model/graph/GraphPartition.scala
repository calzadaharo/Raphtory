package com.raphtory.core.model.graph

import com.raphtory.core.components.akkamanagement.RaphtoryActor._
import com.raphtory.core.implementations.generic.messaging._
import com.raphtory.core.model.graph.visitor.Vertex
import com.typesafe.scalalogging.LazyLogging

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
  * Singleton representing the Storage for the entities
  */

abstract class GraphPartition(partitionID: Int) extends LazyLogging {

  /**
    * Ingesting Vertices
    */

  def addVertex(msgTime: Long, srcId: Long, properties: Properties, vertexType: Option[Type]): Unit

  def removeVertex(msgTime: Long, srcId: Long, channelId: Int, channelTime: Int): List[TrackedGraphEffect[GraphUpdateEffect]]
  def inboundEdgeRemovalViaVertex(msgTime: Long, srcId: Long, dstId: Long, channelId: Int, channelTime: Int): TrackedGraphEffect[GraphUpdateEffect]
  def outboundEdgeRemovalViaVertex(msgTime: Long, srcId: Long, dstId: Long, channelId: Int, channelTime: Int): TrackedGraphEffect[GraphUpdateEffect]

  /**
    * Ingesting Edges
    * */

  def addEdge(msgTime: Long, srcId: Long, dstId: Long, properties: Properties, edgeType: Option[Type], channelId: Int, channelTime: Int): Option[TrackedGraphEffect[GraphUpdateEffect]]
  def syncNewEdgeAdd(msgTime: Long, srcId: Long, dstId: Long, properties: Properties, srcRemovals: List[Long], edgeType: Option[Type], channelId: Int, channelTime: Int): TrackedGraphEffect[GraphUpdateEffect]
  def syncExistingEdgeAdd(msgTime: Long, srcId: Long, dstId: Long, properties: Properties, channelId: Int, channelTime: Int): TrackedGraphEffect[GraphUpdateEffect]

  def removeEdge(msgTime: Long, srcId: Long, dstId: Long, channelId: Int, channelTime: Int): Option[TrackedGraphEffect[GraphUpdateEffect]]
  def syncNewEdgeRemoval(msgTime: Long, srcId: Long, dstId: Long, srcRemovals: List[Long], channelId: Int, channelTime: Int): TrackedGraphEffect[GraphUpdateEffect]
  def syncExistingEdgeRemoval(msgTime: Long, srcId: Long, dstId: Long, channelId: Int, channelTime: Int): TrackedGraphEffect[GraphUpdateEffect]

  def syncExistingRemovals(msgTime: Long, srcId: Long, dstId: Long, dstRemovals: List[Long]): Unit


  def deduplicate()

  /**
    * Analysis Functions
    * */
  def getVertices(graphPerspective: GraphLens, time:Long, window:Long = Long.MaxValue):mutable.Map[Long,Vertex]



  var oldestTime: Long = Long.MaxValue
  var newestTime: Long = 0
  var windowTime: Long = 0

  def timings(updateTime: Long) = {
    if (updateTime < oldestTime && updateTime > 0) oldestTime = updateTime
    if (updateTime > newestTime)
      newestTime = updateTime
  }

   def getPartitionID = partitionID
   def checkDst(dstID: Long): Boolean = (((dstID.abs % totalPartitions )).toInt == partitionID)
}
