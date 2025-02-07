package com.raphtory.core.model.graph

import com.raphtory.core.components.graphbuilder.BuilderExecutor.Message.BuilderOutput


/**
  * Created by Mirate on 30/05/2017.
  */
sealed trait Property {
  def key: String
  def value: Any
}

case class Type(name: String)
case class ImmutableProperty(key: String, value: String) extends Property
case class StringProperty(key: String, value: String)    extends Property
case class LongProperty(key: String, value: Long)        extends Property
case class DoubleProperty(key: String, value: Double)    extends Property
case class FloatProperty(key: String, value: Float)    extends Property
case class Properties(property: Property*)

sealed trait GraphUpdate {
  val updateTime: Long
  val srcId: Long
}

//basic update types
case class VertexAdd(updateTime: Long, srcId: Long, properties: Properties, vType: Option[Type]) extends GraphUpdate //add a vertex (or add/update a property to an existing vertex)
case class VertexDelete(updateTime: Long, srcId: Long) extends GraphUpdate
case class EdgeAdd(updateTime: Long, srcId: Long, dstId: Long, properties: Properties, eType: Option[Type]) extends GraphUpdate
case class EdgeDelete(updateTime: Long, srcId: Long, dstId: Long) extends GraphUpdate

//Required sync after an update has been applied to a partition
sealed abstract class GraphUpdateEffect(val updateId: Long) {val msgTime: Long}

case class SyncNewEdgeAdd(msgTime: Long, srcId: Long, dstId: Long, properties: Properties, removals: List[Long], vType: Option[Type]) extends GraphUpdateEffect(dstId)
case class SyncExistingEdgeAdd(msgTime: Long, srcId: Long, dstId: Long, properties: Properties) extends GraphUpdateEffect(dstId)

case class SyncExistingEdgeRemoval(msgTime: Long, srcId: Long, dstId: Long) extends GraphUpdateEffect(dstId)
case class SyncNewEdgeRemoval(msgTime: Long, srcId: Long, dstId: Long, removals: List[Long]) extends GraphUpdateEffect(dstId)


//Edge removals generated via vertex removals
case class OutboundEdgeRemovalViaVertex(msgTime: Long, srcId: Long, dstId: Long) extends GraphUpdateEffect(dstId)
case class InboundEdgeRemovalViaVertex(msgTime: Long, srcId: Long, dstId: Long) extends GraphUpdateEffect(srcId)

//Responses from a partition receiving any of the above
case class SyncExistingRemovals(msgTime: Long, srcId: Long, dstId: Long, removals: List[Long]) extends GraphUpdateEffect(srcId)
case class EdgeSyncAck(msgTime: Long, srcId: Long) extends GraphUpdateEffect(srcId)
case class VertexRemoveSyncAck(msgTime: Long, override val updateId: Long) extends GraphUpdateEffect(updateId)


//Wrapper to include watermarking channel ID's
case class TrackedGraphUpdate[+T <: GraphUpdate](channelTime:Int, update: T) extends BuilderOutput
case class TrackedGraphEffect[T <: GraphUpdateEffect](channelId: Int, channelTime: Int, effect: T)
case class GraphUpdateBatch[T <: BuilderOutput](channelID:Int, update: Array[T]) extends BuilderOutput
case class GraphSyncBatch[T <: TrackedGraphEffect[GraphUpdateEffect]](updates: Array[T])


case class VertexMessage(superstep:Int,vertexId: Long, data: Any)
case class VertexMessageBatch(data: Array[VertexMessage])
