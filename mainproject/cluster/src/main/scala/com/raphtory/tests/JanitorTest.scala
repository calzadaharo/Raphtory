package com.raphtory.tests

import ch.qos.logback.classic.Level
import com.raphtory.core.model.graphentities.{Edge, Entity, Property, Vertex}
import com.mongodb.casbah.Imports.{$addToSet, _}
import com.mongodb.casbah.MongoConnection
import com.raphtory.core.actors.partitionmanager.MongoFactory
import com.raphtory.core.storage.EntityStorage
import com.raphtory.core.utils.Utils
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.parallel.mutable.ParTrieMap
object JanitorTest extends App{
  val root = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]
  root.setLevel(Level.ERROR)
  val temporalMode = true //flag denoting if storage should focus on keeping more entities in memory or more history
  val timeWindow = 1 //timeWindow set in seconds
  val timeWindowMils = timeWindow * 1000

  //  val vertex = new Vertex(System.currentTimeMillis(),1,true,false)
  //  Thread.sleep(100)
  //  vertex revive(System.currentTimeMillis())
  //  Thread.sleep(100)
  //  vertex revive(System.currentTimeMillis())
  //  Thread.sleep(100)
  //  vertex revive(System.currentTimeMillis())
  //  Thread.sleep(500)
  //  vertex kill(System.currentTimeMillis())
  //  Thread.sleep(500)
  //  vertex revive(System.currentTimeMillis())
  MongoFactory.vertices.drop()
  MongoFactory.edges.drop()

  val vertex = new Vertex(1,1,1,true,false)
  vertex revive(2)
  vertex revive(3)
  vertex revive(4)
  vertex kill(5)
  vertex revive(6)
  vertex +(1,"prop","val1")
  vertex +(2,"prop","val2")
  vertex +(3,"prop","val2")
  vertex +(4,"prop","val3")
  vertex +(5,"prop","val3")
  vertex +(6,"prop","val3")
  vertex +(7,"prop","val3")

  vertex +(1,"prop2","val1")
  vertex +(2,"prop2","val2")
  vertex +(3,"prop2","val2")
  vertex +(4,"prop2","val3")
  vertex +(5,"prop2","val3")
  vertex +(6,"prop2","val3")
  vertex +(7,"prop2","val3")



  vertex addAssociatedEdge new Edge(1,1,1,2,true,false)
  vertex addAssociatedEdge new Edge(1,2,1,3,true,false)
  vertex addAssociatedEdge new Edge(1,3,1,4,true,false)
  vertex addAssociatedEdge new Edge(1,4,1,5,true,false)

  for(edge <- vertex.associatedEdges){
    MongoFactory.edge2Mongo(edge,cutOff)
  }

  //println(cutOff)
  MongoFactory.vertex2Mongo(vertex,cutOff)
  MongoFactory.flushBatch()
  //println(MongoFactory.retrieveVertexRaw(vertex.vertexId))
  //MongoFactory.retrieveVertexHistory(vertex.getId)
  //println(MongoFactory.vertices.find().foreach(x=>println(x.toString)))
  vertex kill(7)
  vertex revive(8)
  vertex +(8,"prop3","dave")
  vertex +(9,"prop3","bob")
 MongoFactory.vertex2Mongo(vertex,cutOff)
 MongoFactory.flushBatch()
 // Thread.sleep(1000)


  //println(vertex.equals(EntityStorage.retrieveVertex(vertex.getId.toInt)))
  println(new Edge(1,1,1,2,true,false).getId)
  println(MongoFactory.retrieveEdge(new Edge(1,1,1,2,true,false).getId))
  println(MongoFactory.retrieveEdgeRaw(new Edge(1,1,1,2,true,false).getId))
  try{println(EntityStorage.retrieveEdge(new Edge(1,1,1,2,true,false).getId))}catch {case e:Exception => println("broke")}
  //println(new Edge(1,1,1,2,true,false).equals(EntityStorage.retrieveEdge(new Edge(1,1,1,2,true,false).getId)))
 // MongoFactory.vertices.drop()
 // MongoFactory.edges.drop()



  def compressHistory(e:Entity) ={
    val compressedHistory = e.compressAndReturnOldHistory(cutOff)
    for ((id,property) <- e.properties){
      val oldHistory = property.compressAndReturnOldHistory(cutOff)
      // savePropertiesToRedis(e, past)
      //store offline
    }
  }

  def cutOff = System.currentTimeMillis()+1000
//  object MongoFactory {
//    private val DATABASE = "raphtory"
//    val connection = MongoConnection()
//    val edges = connection(DATABASE)("edges")
//    val vertices = connection(DATABASE)("vertices")
//  }
//
//  def entity2Mongo(entity:Entity)={
//    if(entity beenSaved()){
//      update(entity)
//    }
//    else{
//      newEntity(entity)
//    }
//
//  }

//  def update(entity: Entity) ={
//    val history = convertHistoryUpdate(entity.compressAndReturnOldHistory(cutOff))
//    //MongoFactory.vertices.update(DBObject("_id" -> entity.getId), $addToSet("history") $each(history: _*))
//    //MongoFactory.vertices.find(MongoDBObject("_id" -> 1)) .updateOne($addToSet("history") $each(history: _*))
//      val builder = MongoFactory.vertices.initializeOrderedBulkOperation
//      val dbEntity = builder.find(MongoDBObject("_id" -> entity.getId))
//      dbEntity.updateOne($addToSet("history") $each(history:_*))
//      for((key,property) <- entity.properties){
//        val entityHistory = convertHistoryUpdate(property.compressAndReturnOldHistory(cutOff))
//        println(entityHistory)
//        if(history.nonEmpty)
//          dbEntity.updateOne($addToSet(s"properties.$key") $each(entityHistory:_*))
//      }
//
//      val result = builder.execute()
//  }
//
//  def newEntity(entity:Entity) ={
//    val history = entity.compressAndReturnOldHistory(cutOff)
//    val builder = MongoDBObject.newBuilder
//    builder += "_id" -> entity.getId
//    builder += "oldestPoint" -> entity.oldestPoint.get
//    builder += "history" -> convertHistory(history)
//    builder += "properties" -> convertProperties(entity.properties)
//    MongoFactory.vertices.save(builder.result())
//  }
//
//  def convertHistory[b <: Any](history:mutable.TreeMap[Long,b]):MongoDBList ={
//    val builder = MongoDBList.newBuilder
//    for((k,v) <-history)
//      if(v.isInstanceOf[String])
//        builder += MongoDBObject("time"->k,"value"->v.asInstanceOf[String])
//      else
//        builder += MongoDBObject("time"->k,"value"->v.asInstanceOf[Boolean])
//
//    builder.result
//  }
//  def convertHistoryUpdate[b <: Any](history:mutable.TreeMap[Long,b]):List[MongoDBObject] ={
//    val builder = mutable.ListBuffer[MongoDBObject]()
//    for((k,v) <-history)
//      if(v.isInstanceOf[String])
//        builder += MongoDBObject("time"->k,"value"->v.asInstanceOf[String])
//      else
//        builder += MongoDBObject("time"->k,"value"->v.asInstanceOf[Boolean])
//
//    builder.toList
//  }
//
//  def convertProperties(properties: ParTrieMap[String,Property]):MongoDBObject = {
//    val builder = MongoDBObject.newBuilder
//    for ((k, v) <- properties) {
//      builder += k -> convertHistory(v.compressAndReturnOldHistory(cutOff))
//    }
//    builder.result
//  }

 // def convertProperty(property:Property):MongoDBObject ={
 //   val builder = MongoDBObject.newBuilder
 //   builder.result()
 // }



 // MongoFactory.collection.update(DBObject("_id" -> "1"), $addToSet("list") $each (Seq("1"->"true", "2"->false, "3"->true ): _*))
  //MongoFactory.collection

  //vertex + (1, "Name", "Ben")
  //vertex + (1, "Hair", "Brown")

  // vertex + (3, "Eyes", "Brown")
  // vertex + (1, "Name", "Alessandro")
//  val builder = collection.initializeOrderedBulkOperation
//  builder.insert(MongoDBObject("_id" -> 1))
//  builder.insert(MongoDBObject("_id" -> 2))
//  builder.insert(MongoDBObject("_id" -> 3))
//
//  builder.find(MongoDBObject("_id" -> 1)).updateOne($set("x" -> 2))
//  builder.find(MongoDBObject("_id" -> 2)).removeOne()
//  builder.find(MongoDBObject("_id" -> 3)).replaceOne(MongoDBObject("_id" -> 3, "x" -> 4))
//
//  val result = builder.execute()

}


//  def saveToRedis(compressedHistory : mutable.TreeMap[Long, Boolean], entityType : KeyEnum.Value, entityId : Long, pastCheckpoint : Long, e :Entity) = {
//    RedisConnector.addEntity(entityType, entityId, e.creationTime)
//    for ((k,v) <- compressedHistory) {
//      if (k > pastCheckpoint)
//        RedisConnector.addState(entityType, entityId,k, v)
//    }
//  }
//
//  def savePropertiesToRedis(e : Entity, pastCheckpoint : Long) = {
//    val properties = e.properties
//    var entityType = KeyEnum.edges
//    val id         = e.getId
//    if (e.isInstanceOf[Vertex])
//        entityType = KeyEnum.vertices
//
//    properties.foreach(el => {
//      val propValue = el._2
//      val propName  = el._1
//      propValue.previousState.foreach(h => {
//        if (h._1 > pastCheckpoint)
//          RedisConnector.addProperty(entityType, id, propName, h._1, h._2)
//        else break
//      })
//    })
//  }