package com.raphtory.core.components.graphbuilder


import java.nio.ByteBuffer
import com.raphtory.core.implementations.generic.messaging._
import com.raphtory.core.model.graph.{EdgeAdd, EdgeDelete, GraphUpdate, Properties, Type, VertexAdd, VertexDelete}
import net.jpountz.xxhash.XXHash64
import net.openhft.hashing
import net.openhft.hashing.LongHashFunction

import scala.util.hashing.MurmurHash3

trait GraphBuilder[T] {

  private var updates: List[GraphUpdate] = List.empty

  private[graphbuilder] def getUpdates(tuple: T): List[GraphUpdate] = {
    try parseTuple(tuple)
    catch {
      case e: Exception => println(s"Tuple broken: $tuple")
    }
    val toReturn = updates
    updates = List.empty
    toReturn
  }

  protected def assignID(uniqueChars: String): Long = LongHashFunction.xx3().hashChars(uniqueChars)

  protected def parseTuple(tuple: T): Unit

  // Graph support methods
  protected def addVertex(updateTime: Long, srcId: Long): Unit =
    updates = updates :+ VertexAdd(updateTime, srcId, Properties(), None)

  protected def addVertex(updateTime: Long, srcId: Long, properties: Properties): Unit =
    updates = updates :+ VertexAdd(updateTime, srcId, properties, None)

  protected def addVertex(updateTime: Long, srcId: Long, vertexType: Type): Unit =
    updates = updates :+ VertexAdd(updateTime, srcId, Properties(), Some(vertexType))

  protected def addVertex(updateTime: Long, srcId: Long, properties: Properties, vertexType: Type): Unit =
    updates = updates :+ VertexAdd(updateTime, srcId, properties, Some(vertexType))

  protected def deleteVertex(updateTime: Long, srcId: Long): Unit =
    updates = updates :+ VertexDelete(updateTime, srcId)

  protected def addEdge(updateTime: Long, srcId: Long, dstId: Long): Unit =
    updates = updates :+ EdgeAdd(updateTime, srcId, dstId, Properties(), None)

  protected def addEdge(updateTime: Long, srcId: Long, dstId: Long, properties: Properties): Unit =
    updates = updates :+ EdgeAdd(updateTime, srcId, dstId, properties, None)

  protected def addEdge(updateTime: Long, srcId: Long, dstId: Long, edgeType: Type): Unit =
    updates = updates :+ EdgeAdd(updateTime, srcId, dstId, Properties(), Some(edgeType))

  protected def addEdge(updateTime: Long, srcId: Long, dstId: Long, properties: Properties, edgeType: Type): Unit =
    updates = updates :+ EdgeAdd(updateTime, srcId, dstId, properties, Some(edgeType))

  protected def deleteEdge(updateTime: Long, srcId: Long, dstId: Long): Unit =
    updates = updates :+ EdgeDelete(updateTime, srcId, dstId)
}
