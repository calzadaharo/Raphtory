package com.raphtory.core.model.algorithm




sealed trait TableFunction

case class TableFilter(f:(Row)=>Boolean) extends TableFunction
case class WriteTo(address:String)       extends TableFunction
case class Explode(f:Row=>List[Row])     extends TableFunction

abstract class Table {
  def filter(f:Row=>Boolean):Table
  def explode(f:Row=>List[Row]):Table
  def writeTo(address:String)
}


