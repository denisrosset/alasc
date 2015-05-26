package net.alasc
package math
package enum

/** An indexed sequence that allows for greater lengths than Int.MaxValue; only a few
  * methods of IndexedSeq are defined as of now.
  */ 
trait BigIndexedSeq[A] extends PartialFunction[BigInt, A] with Iterable[A] { self =>
  def length: BigInt
  def apply(idx: BigInt): A
  def isDefinedAt(idx: BigInt): Boolean = (idx >= 0 && idx < length)
  def map[B](f: A => B): BigIndexedSeq[B] = new MappedBigIndexedSeq[A, B](self, f)
}

final class MappedBigIndexedSeq[A, B](original: BigIndexedSeq[A], f: A => B) extends BigIndexedSeq[B] {
  def length = original.length
  def iterator = original.iterator.map(f)
  def apply(idx: BigInt): B = f(original(idx))
  override def map[C](g: B => C): BigIndexedSeq[C] = new MappedBigIndexedSeq(original, f andThen g)
}
