package net.alasc

trait BigSeq[+A] extends PartialFunction[BigInt, A] {
  def length: BigInt
  def iterator: Iterator[A]
  def apply(index: BigInt): A

  def size = length
  def isDefinedAt(index: BigInt) = (0 <= index) && (index < length)
}
