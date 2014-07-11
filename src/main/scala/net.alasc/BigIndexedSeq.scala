package net.alasc

trait BigIndexedSeq[+A] extends PartialFunction[BigInt, A] {
  self =>
  def head: A
  def length: BigInt
  def iterator: Iterator[A]
  def apply(index: BigInt): A
  def map[B](f: A => B): BigIndexedSeq[B] = new MappedBigIndexedSeq(self, f)
  def size = length
  def isDefinedAt(index: BigInt) = (0 <= index) && (index < length)
}

protected class MappedBigIndexedSeq[A, B](original: BigIndexedSeq[A], f: A => B) extends BigIndexedSeq[B] {
  def head = f(original.head)
  def length = original.length
  def iterator = original.iterator.map(f)
  def apply(index: BigInt) = f(original(index))
}
