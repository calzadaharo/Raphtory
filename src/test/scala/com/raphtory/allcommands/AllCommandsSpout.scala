package com.raphtory.allcommands

import com.raphtory.core.components.spout.Spout

class AllCommandsSpout extends Spout[String] {

  var totalCount = 100
  var freq = 1000
  var pool: Int = 10000
  var time: Int = 300000
  var vprob: Double = 0.3
  var eprob: Double = 0.7
  var msgID = 0
  val rnd = new scala.util.Random(123)
  override def setupDataSource(): Unit = {}

  override def generateData(): Option[String] ={
    if(msgID<=time)
      Some(distribution())
    else {
      dataSourceComplete()
      None
    }
  }


  override def closeDataSource(): Unit = {}


  def distribution(): String = {
    val random = rnd.nextFloat()
    // genVertexAdd()
//    if (random <= vprob) genVertexAdd()
    //else genEdgeAdd()
//    else
      if (random <= eprob) genEdgeAdd()
    //  else genEdgeRemoval()
    else if (random <= 0.98) genEdgeRemoval()
    else genEdgeRemoval()//genVertexRemoval()
  }

  def genVertexAdd(): String =
    s""" {"VertexAdd":{${getMessageID()}, ${genSrcID()}, ${genProperties(1)}}}"""

  def genVertexRemoval(): String =
    s""" {"VertexRemoval":{${getMessageID()}, ${genSrcID()}}}"""

  def genEdgeAdd(): String =
    s""" {"EdgeAdd":{${getMessageID()}, ${genSrcID()}, ${genDstID()}, ${genProperties(1)}}}"""

  def genEdgeRemoval(): String =
    s""" {"EdgeRemoval":{${getMessageID()}, ${genSrcID()}, ${genDstID()}}}"""

  def genSrcID(): String = s""" "srcID":${rnd.nextInt(pool)} """

  def genDstID(): String = s""" "dstID":${rnd.nextInt(pool)} """

  def getMessageID(): String = {
    msgID += 1
    s""" "messageID":$msgID """
  }

  def genProperties(numOfProps: Int): String = {
    var properties = "\"properties\":{"
    for (i <- 1 to numOfProps) {
      val propnum = i
      if (i < numOfProps) properties = properties + s""" "property$propnum":"${"test"}", """
      else properties = properties + s""" "property$propnum":${rnd.nextFloat()} }"""
    }
    properties
  }
}