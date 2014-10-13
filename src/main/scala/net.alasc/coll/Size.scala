package net.alasc.coll

sealed trait Size {
  def toInt: Int
  def toBigInt: BigInt
}
case class IntSize(size: Int) extends Size {
  def toInt = size
  def toBigInt = BigInt(size)
}
case class BigIntSize(size: BigInt) extends Size {
  def toInt = {
    require(size.isValidInt)
    size.toInt
  }
  def toBigInt = size
}
case object InfiniteSize extends Size {
  def toInt = sys.error("Cannot return infinite size as Int")
  def toBigInt = sys.error("Cannot return infinite size as BigInt")
}
