package net.alasc.util

class Tuple2Int(val encoding: Long) extends AnyVal {
  import Tuple2Int._
  def _1: Int = (encoding & rightMask).toInt
  def _2: Int = ((encoding & leftMask) >> 32).toInt
  def isEmpty: Boolean = false
  def get: Tuple2Int = this
}

object Tuple2Int {
  def unapply(tuple2: Tuple2Int): Tuple2Int = tuple2
  def apply(_1: Int, _2: Int): Tuple2Int = new Tuple2Int(((_1.toLong) & rightMask) + (_2.toLong << 32))
  @inline def leftMask: Long  = (0xFFFFFFFFL) << 32
  @inline def rightMask: Long = 0xFFFFFFFFL
}
