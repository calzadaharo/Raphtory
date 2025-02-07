package com.raphtory.core.components.leader

import java.util.concurrent.atomic.AtomicLong
import akka.actor.ActorRef
import akka.cluster.pubsub.{DistributedPubSub, DistributedPubSubMediator}
import com.raphtory.core.components.akkamanagement.RaphtoryActor._
import WatermarkManager.Message.{ProbeWatermark, WatermarkTime, WhatsTheTime}
import com.raphtory.core.components.leader.WatchDog.Message.{ClusterStatusRequest, ClusterStatusResponse}
import com.raphtory.core.components.akkamanagement.RaphtoryActor
import com.typesafe.config.Config

import java.time.LocalDateTime
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class WatermarkManager(watchDog: ActorRef) extends RaphtoryActor  {

  var clusterUp = false

  override def preStart(): Unit = {
    context.system.scheduler.scheduleOnce(delay = 5.seconds, receiver = self, message = "clusterUp")
  }

  private val safeMessageMap = mutable.Map[String, Long]()
  var counter = 0;

  override def receive: Receive = {
    case "clusterUp"     => watchDog ! ClusterStatusRequest
    case ClusterStatusResponse(clusterUp) =>
      if(clusterUp)
        probeWatermark()
      else
        context.system.scheduler.scheduleOnce(delay = 5.seconds, receiver = self, message = "clusterUp")
    case "probe"         => probeWatermark()
    case u:WatermarkTime => processWatermarkTime(u)
    case WhatsTheTime    =>
      val time = if(safeMessageMap.size == RaphtoryActor.totalPartitions) safeMessageMap.minBy(x=> x._2)._2 else 0L
      sender() ! WatermarkTime(time)
  }

  def probeWatermark() = {
    getAllWriters().foreach { workerPath =>
      mediator ! new DistributedPubSubMediator.Send(
        workerPath,
        ProbeWatermark
      )
    }
  }

  def processWatermarkTime(u:WatermarkTime):Unit = {
    safeMessageMap put(sender().path.toString,u.time)
    counter +=1
    if(counter==totalPartitions) {
      val max = safeMessageMap.maxBy(x=> x._2)
      val min = safeMessageMap.minBy(x=> x._2)
      log.info(s"Minimum Watermark: ${min._1} ${min._2} Maximum Watermark: ${max._1} ${max._2}")
      context.system.scheduler.scheduleOnce(delay = 1.seconds, receiver = self, message = "probe")
      counter=0
    }
  }

}

object WatermarkManager {
  object Message {
    case object ProbeWatermark
    case class WatermarkTime(time:Long)
    case object SaveState
    case object WhatsTheTime
  }
}