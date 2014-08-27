package net.alasc.coll

sealed trait Size
case class IntSize(size: Int) extends Size
case class BigIntSize(size: BigInt) extends Size
case object InfiniteSize extends Size

trait HasSize extends Any {
  def size: Size
}
