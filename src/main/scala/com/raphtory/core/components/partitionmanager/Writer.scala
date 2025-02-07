package com.raphtory.core.components.partitionmanager

import akka.actor.{ActorRef, Cancellable}
import akka.cluster.pubsub.{DistributedPubSub, DistributedPubSubMediator}
import com.raphtory.core.components.graphbuilder.BuilderExecutor.Message.{BuilderOutput, BuilderTimeSync, PartitionRequest}
import com.raphtory.core.components.partitionmanager.Writer.Message.{Dedupe, EffectPublish, Watermark}
import com.raphtory.core.components.akkamanagement.RaphtoryActor._
import com.raphtory.core.components.akkamanagement.{MailboxTrackedActor, RaphtoryActor}
import com.raphtory.core.components.leader.WatermarkManager.Message.{ProbeWatermark, WatermarkTime}
import com.raphtory.core.implementations.generic.messaging._
import com.raphtory.core.model.graph.{EdgeAdd, EdgeDelete, EdgeSyncAck, GraphPartition, GraphSyncBatch, GraphUpdateBatch, GraphUpdateEffect, InboundEdgeRemovalViaVertex, OutboundEdgeRemovalViaVertex, SyncExistingEdgeAdd, SyncExistingEdgeRemoval, SyncExistingRemovals, SyncNewEdgeAdd, SyncNewEdgeRemoval, TrackedGraphEffect, TrackedGraphUpdate, VertexAdd, VertexDelete, VertexRemoveSyncAck}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._


final class Writer(partitionID:Int, storage: GraphPartition) extends RaphtoryActor with MailboxTrackedActor {

  private var increments = 0
  private var updates = 0
  private var updates2 = 0
  private var vertexAdds = 0

  private var pullCount = 0
  private var updatesBefore = 0

  private val queuedMessageMap = mutable.Map[Int, mutable.PriorityQueue[queueItem]]()
  private val safeMessageMap = mutable.Map[Int, queueItem]()
  private val vDeleteCountdownMap = mutable.Map[(Int, Int), AtomicInteger]()

  private val updateCache = mutable.Map[String, mutable.ArrayBuffer[TrackedGraphEffect[GraphUpdateEffect]]]()
  getAllWriters().foreach(name => updateCache.put(name, mutable.ArrayBuffer[TrackedGraphEffect[GraphUpdateEffect]]()))

  override def receive: Receive = mailboxTrackedReceive {

    case GraphUpdateBatch(channelID,updates) =>
      updates.foreach(update => update match {
        case TrackedGraphUpdate(channelTime, req: VertexAdd) => processVertexAddRequest(channelID, channelTime, req) //Add a new vertex
        case TrackedGraphUpdate(channelTime, req: EdgeAdd) => processEdgeAddRequest(channelID, channelTime, req) //Add an edge
        case TrackedGraphUpdate(channelTime, req: EdgeDelete) => processEdgeDelete(channelID, channelTime, req) //Delete an Edge
        case TrackedGraphUpdate(channelTime, req: VertexDelete) => processVertexDelete(channelID, channelTime, req) //Delete a vertex and all associated edges
        case req: BuilderTimeSync => processBuilderTimeSync(req);

      })

    case TrackedGraphEffect(channelId, channelTime, req: SyncNewEdgeAdd) => processSyncNewEdgeAdd(channelId, channelTime, req) //A writer has requested a new edge sync for a destination node in this worker
    case TrackedGraphEffect(channelId, channelTime, req: SyncExistingEdgeAdd) => processSyncExistingEdgeAdd(channelId, channelTime, req) // A writer has requested an existing edge sync for a destination node on in this worker
    case TrackedGraphEffect(channelId, channelTime, req: SyncExistingRemovals) => processSyncExistingRemovals(channelId, channelTime, req) //The remote worker has returned all removals in the destination node -- for new edges
    case TrackedGraphEffect(channelId, channelTime, req: EdgeSyncAck) => processEdgeSyncAck(channelId, channelTime, req) //The remote worker acknowledges the completion of an edge sync

    case TrackedGraphEffect(channelId, channelTime, req: SyncNewEdgeRemoval) => processSyncNewEdgeRemoval(channelId, channelTime, req) //A remote worker is asking for a new edge to be removed for a destination node in this worker
    case TrackedGraphEffect(channelId, channelTime, req: SyncExistingEdgeRemoval) => processSyncExistingEdgeRemoval(channelId, channelTime, req) //A remote worker is asking for the deletion of an existing edge

    case TrackedGraphEffect(channelId, channelTime, req: OutboundEdgeRemovalViaVertex) => processOutboundEdgeRemovalViaVertex(channelId, channelTime, req) //Does exactly the same as above, but for when the removal comes form a vertex
    case TrackedGraphEffect(channelId, channelTime, req: InboundEdgeRemovalViaVertex) => processInboundEdgeRemovalViaVertex(channelId, channelTime, req) // Excatly the same as above, but for a remote worker

    case TrackedGraphEffect(channelId, channelTime, req: VertexRemoveSyncAck) => processVertexRemoveSyncAck(channelId, channelTime, req)

    case GraphSyncBatch(updates) =>
      updates.foreach(update => update match {
        case TrackedGraphEffect(channelId, channelTime, req: SyncNewEdgeAdd) => processSyncNewEdgeAdd(channelId, channelTime, req) //A writer has requested a new edge sync for a destination node in this worker
        case TrackedGraphEffect(channelId, channelTime, req: SyncExistingEdgeAdd) => processSyncExistingEdgeAdd(channelId, channelTime, req) // A writer has requested an existing edge sync for a destination node on in this worker
        case TrackedGraphEffect(channelId, channelTime, req: SyncExistingRemovals) => processSyncExistingRemovals(channelId, channelTime, req) //The remote worker has returned all removals in the destination node -- for new edges
        case TrackedGraphEffect(channelId, channelTime, req: EdgeSyncAck) => processEdgeSyncAck(channelId, channelTime, req) //The remote worker acknowledges the completion of an edge sync

        case TrackedGraphEffect(channelId, channelTime, req: SyncNewEdgeRemoval) => processSyncNewEdgeRemoval(channelId, channelTime, req) //A remote worker is asking for a new edge to be removed for a destination node in this worker
        case TrackedGraphEffect(channelId, channelTime, req: SyncExistingEdgeRemoval) => processSyncExistingEdgeRemoval(channelId, channelTime, req) //A remote worker is asking for the deletion of an existing edge

        case TrackedGraphEffect(channelId, channelTime, req: OutboundEdgeRemovalViaVertex) => processOutboundEdgeRemovalViaVertex(channelId, channelTime, req) //Does exactly the same as above, but for when the removal comes form a vertex
        case TrackedGraphEffect(channelId, channelTime, req: InboundEdgeRemovalViaVertex) => processInboundEdgeRemovalViaVertex(channelId, channelTime, req) // Excatly the same as above, but for a remote worker

        case TrackedGraphEffect(channelId, channelTime, req: VertexRemoveSyncAck) => processVertexRemoveSyncAck(channelId, channelTime, req)
      })

    case Watermark => processWatermarkRequest(); //println(s"$workerId ${storage.newestTime} ${storage.windowTime} ${storage.newestTime-storage.windowTime}")
    case EffectPublish => sendEffectMessages()
    case Dedupe => dedupe()
    case ProbeWatermark => mediator ! DistributedPubSubMediator.Send("/user/WatermarkManager", WatermarkTime(storage.windowTime), localAffinity = false)
    //case SaveState => serialiseGraphPartition();
    case x => log.warning(s"IngestionWorker [{}] received unknown [{}] message.", partitionID, x)
  }



  def processVertexAddRequest(channelId: Int, channelTime: Int, update: VertexAdd): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$update] request.")
    storage.addVertex(update.updateTime, update.srcId, update.properties, update.vType)

    trackVertexAdd(update.updateTime, channelId, channelTime)
  }

  private def trackVertexAdd(msgTime: Long, channelId: Int, channelTime: Int): Unit = {
    //Vertex Adds the message time straight into queue as no sync
    storage.timings(msgTime)
    vertexAdds+=1
    addToWatermarkQueue(channelId, channelTime, msgTime)
  }


  def processEdgeAddRequest(channelId: Int, channelTime: Int, update: EdgeAdd): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$update] request.")
    val maybeEffect = storage.addEdge(update.updateTime, update.srcId, update.dstId, update.properties, update.eType, channelId, channelTime)
    maybeEffect.foreach(queueEffectMessage)
    trackEdgeAdd(update.updateTime, maybeEffect.isEmpty, channelId, channelTime)
  }

  private def trackEdgeAdd(msgTime: Long, local: Boolean, channelId: Int, channelTime: Int): Unit = {
    storage.timings(msgTime)
    if (local) { //if the edge is totally handled by this worker then we are safe to add to watermark queue
      addToWatermarkQueue(channelId, channelTime, msgTime)
    }
  }

  def processSyncExistingEdgeAdd(channelId: Int, channelTime: Int, req: SyncExistingEdgeAdd): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$req] request.")
    val effect = storage.syncExistingEdgeAdd(req.msgTime, req.srcId, req.dstId, req.properties, channelId, channelTime)
    queueEffectMessage(effect)
    remoteEdgeAddTrack(req.msgTime)
  }

  def processSyncNewEdgeAdd(channelId: Int, channelTime: Int, req: SyncNewEdgeAdd): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$req] request.")
    val effect = storage.syncNewEdgeAdd(req.msgTime, req.srcId, req.dstId, req.properties, req.removals, req.vType, channelId, channelTime)
    queueEffectMessage(effect)
    remoteEdgeAddTrack(req.msgTime)
  }

  private def remoteEdgeAddTrack(msgTime: Long): Unit = {
    storage.timings(msgTime)
  }

  //
  def processSyncExistingRemovals(channelId: Int, channelTime: Int, req: SyncExistingRemovals): Unit = { //when the new edge add is responded to we can say it is synced
    log.debug(s"IngestionWorker [$partitionID] received [$req] request.")
    storage.syncExistingRemovals(req.msgTime, req.srcId, req.dstId, req.removals)
    addToWatermarkQueue(channelId, channelTime, req.msgTime)
  }

  def processEdgeSyncAck(channelId: Int, channelTime: Int, req: EdgeSyncAck) = { //when the edge isn't new we will get this response instead
    log.debug(s"IngestionWorker [$partitionID] received [$req] request.")
    addToWatermarkQueue(channelId, channelTime, req.msgTime)
  }


  def processEdgeDelete(channelId: Int, channelTime: Int, update: EdgeDelete): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$update] request.")
    val maybeEffect = storage.removeEdge(update.updateTime, update.srcId, update.dstId, channelId, channelTime)
    maybeEffect.foreach(queueEffectMessage)
    trackEdgeDelete(update.updateTime, maybeEffect.isEmpty, channelId, channelTime)
  }

  private def trackEdgeDelete(msgTime: Long, local: Boolean, channelId: Int, channelTime: Int): Unit = {
    storage.timings(msgTime)
    if (local) { //if the edge is totally handled by this worker then we are safe to add to watermark queue
      addToWatermarkQueue(channelId, channelTime, msgTime)
    }
  }

  def processSyncNewEdgeRemoval(channelId: Int, channelTime: Int, req: SyncNewEdgeRemoval): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$req] request.")
    val effect = storage.syncNewEdgeRemoval(req.msgTime, req.srcId, req.dstId, req.removals, channelId, channelTime)
    queueEffectMessage(effect)
    storage.timings(req.msgTime)
  }

  def processSyncExistingEdgeRemoval(channelId: Int, channelTime: Int, req: SyncExistingEdgeRemoval): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$req] request.")
    val effect = storage.syncExistingEdgeRemoval(req.msgTime, req.srcId, req.dstId, channelId, channelTime)
    queueEffectMessage(effect)
    storage.timings(req.msgTime)
  }


  def processVertexDelete(channelId: Int, channelTime: Int, update: VertexDelete): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$update] request.")
    val messages = storage.removeVertex(update.updateTime, update.srcId, channelId, channelTime)
    messages.foreach(x => queueEffectMessage(x))
    trackVertexDelete(update.updateTime, channelId, channelTime, messages.size)
  }

  private def trackVertexDelete(msgTime: Long, channelId: Int, channelTime: Int, totalCount: Int): Unit = {
    if (totalCount == 0) //if there are no outgoing edges it is safe to watermark
      addToWatermarkQueue(channelId, channelTime, msgTime)
    else {
      vDeleteCountdownMap put((channelId, channelTime), new AtomicInteger(totalCount))
    }
    storage.timings(msgTime)
  }

  def processVertexRemoveSyncAck(channelId: Int, channelTime: Int, req: VertexRemoveSyncAck) = {
    vDeleteCountdownMap.get((channelId, channelTime)) match {
      case Some(integer) => if (integer.decrementAndGet() == 0) {
        addToWatermarkQueue(channelId, channelTime, req.msgTime)
        vDeleteCountdownMap.remove((channelId, channelTime)) //todo improve this datastructure
      }
      case None => log.error(s"$channelId $channelTime $req does not match records in vDeleteCountdownMap")
    }
  }

  def processOutboundEdgeRemovalViaVertex(channelId: Int, channelTime: Int, req: OutboundEdgeRemovalViaVertex): Unit = {
    log.debug(s"IngestionWorker [$partitionID] received [$req] request.")
    val effect = storage.outboundEdgeRemovalViaVertex(req.msgTime, req.srcId, req.dstId, channelId, channelTime)
    queueEffectMessage(effect)
    storage.timings(req.msgTime)
  }

  def processInboundEdgeRemovalViaVertex(channelId: Int, channelTime: Int, req: InboundEdgeRemovalViaVertex): Unit = { //remote worker same as above
    log.debug(s"IngestionWorker [$partitionID] received [$req] request.")
    val effect = storage.inboundEdgeRemovalViaVertex(req.msgTime, req.srcId, req.dstId, channelId, channelTime)
    queueEffectMessage(effect)
  }

  private def addToWatermarkQueue(channelId: Int, channelTime: Int, msgTime: Long) = {
    queuedMessageMap.get(channelId) match {
      case Some(queue) => queue += queueItem(channelTime, msgTime)
      case None =>
        val queue = new mutable.PriorityQueue[queueItem]()(Ordering.by[queueItem, Int](f => f.builderEpoch).reverse)
        queue += queueItem(channelTime, msgTime)
        queuedMessageMap put(channelId, queue)
    }
    updates += 1
  }


  private def processWatermarkRequest() = {
    if (queuedMessageMap nonEmpty) {
      val queueState = queuedMessageMap.map(queue => {
        setSafePoint(queue._1, queue._2)
      })
      val timestamps = queueState.map(q => q.timestamp)
      updates2 = updates
      increments += 1

      val min = timestamps.min
      if (storage.windowTime < min)
        storage.windowTime = min
    }
  }

  private def dedupe():Unit = {
    storage.deduplicate()
    scheduleTaskOnce(3 minute, receiver = self, message = Dedupe)
  }

  import scala.util.control.Breaks._

  private def setSafePoint(builderName: Int, messageQueue: mutable.PriorityQueue[queueItem]) = {

    var currentSafePoint = safeMessageMap.get(builderName) match {
      case Some(value) => value
      case None => queueItem(-1, 0)
    }
    breakable {
      while (messageQueue nonEmpty)
        if (messageQueue.head.builderEpoch == currentSafePoint.builderEpoch + 1)
          currentSafePoint = messageQueue.dequeue()
        else if (messageQueue.head.builderEpoch == currentSafePoint.builderEpoch)
          currentSafePoint = messageQueue.dequeue()
        else {
          break
        }
    }
    safeMessageMap put(builderName, currentSafePoint)
    currentSafePoint
  }

  private def processBuilderTimeSync(req: BuilderTimeSync) = {
    storage.timings(req.msgTime)
    addToWatermarkQueue(req.BuilderId, req.builderTime, req.msgTime)
  }

  private def queueEffectMessage(msg: TrackedGraphEffect[GraphUpdateEffect]): Unit = {
    //mediator ! new DistributedPubSubMediator.Send(getWriter(msg.effect.updateId), msg)
    updateCache(getWriter(msg.effect.updateId)) += msg
  }

  private def sendEffectMessages():Unit = {
    updateCache.foreach{
      case (partitionName,queue) =>
        mediator ! new DistributedPubSubMediator.Send(partitionName, GraphSyncBatch(queue.toArray))
    }
    getAllWriters().foreach(name => updateCache.put(name, mutable.ArrayBuffer[TrackedGraphEffect[GraphUpdateEffect]]()))
    scheduleTaskOnce(1 second, receiver = self, message = EffectPublish)

  }


  private val scheduledTaskMap: mutable.HashMap[String, Cancellable] = mutable.HashMap[String, Cancellable]()

  override def preStart() {
    log.debug("IngestionWorker is being started.")
    scheduleTasks()
  }

  override def postStop() {
    val allTasksCancelled = scheduledTaskMap.forall {
      case (key, task) =>
        cancelTask(key, task)
    }
    if (!allTasksCancelled) log.warning("Failed to cancel all scheduled tasks post stop.")
  }

  private def scheduleTasks(): Unit = {
    val watermarkCancellable =
      scheduleTask(initialDelay = 1 seconds, interval = 1 second, receiver = self, message = Watermark)
    scheduledTaskMap.put("watermark", watermarkCancellable)

    scheduleTaskOnce(10 seconds, receiver = self, message = EffectPublish)
    scheduleTaskOnce(10 seconds, receiver = self, message = Dedupe)
  }

  case class queueItem(builderEpoch:Int, timestamp:Long)extends Ordered[queueItem] {
    def compare(that: queueItem): Int = that.builderEpoch-this.builderEpoch
  }
}

object Writer {
  object Message {
    case object Watermark
    case object EffectPublish
    case object Dedupe
  }
}
//  def serialiseGraphPartition() = {
//    ParquetWriter.writeAndClose(s"/Users/Mirate/github/test/$partitionID/$workerId/graph.parquet", storage.vertices.map(x=>x._2.serialise()).toArray.toIterable)
//    ParquetWriter.writeAndClose(s"/Users/Mirate/github/test/$partitionID/$workerId/state.parquet", List(StorageState(storage.managerCount,storage.oldestTime,storage.newestTime,storage.windowTime)).toIterable)
//  }
//  def deserialiseGraphPartitions() = {
//    val graph = ParquetReader.read[ParquetVertex](s"/Users/Mirate/github/test/$partitionID/$workerId/graph.parquet")
//    try {
//      graph.foreach(vertex => storage.vertices+=((vertex.id,RaphtoryVertex(vertex))))
//    } finally graph.close()
//
//    val state = ParquetReader.read[StorageState](s"/Users/Mirate/github/test/$partitionID/$workerId/graph.parquet")
//    try {
//      state.foreach(stats => {
//        storage.managerCount=stats.managerCount
//        storage.oldestTime=stats.oldestTime
//        storage.newestTime=stats.newestTime
//        storage.windowTime=stats.windowTime
//      })
//    } finally state.close()
//  }

//case class ParquetProperty(key:String,immutable:Boolean,history:List[(Long,String)])
//case class ParquetEdge(src:Long, dst:Long, split:Boolean, history:List[(Long,Boolean)], properties:List[ParquetProperty])
//case class ParquetVertex(id:Long, history:List[(Long,Boolean)], properties:List[ParquetProperty], incoming:List[ParquetEdge], outgoing:List[ParquetEdge])
//case class StorageState(managerCount:Int,oldestTime: Long,newestTime: Long,windowTime: Long )


