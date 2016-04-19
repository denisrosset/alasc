package net.alasc
package algebra

import spire.math.SafeLong
import spire.syntax.order._
import spire.util.Opt

/** An indexed sequence that allows for greater lengths than Int.MaxValue; only a few
  * methods of IndexedSeq are defined as of now.
  */ 
trait BigIndexedSeq[A] extends PartialFunction[SafeLong, A] with Iterable[A] { self =>
  override def toString = s"BigIndexedSeq($head, ... total of $length elements)"
  def length: SafeLong
  override def size: Int = {
    val l = length
    if (l.isValidInt) l.toInt else sys.error(s"$l is too big to be a valid Int")
  }
  def apply(idx: SafeLong): A
  def isDefinedAt(idx: SafeLong): Boolean = (idx >= 0 && idx < length)
  def indexOf(a: A): Opt[SafeLong] = {
    var idx = SafeLong.zero
    val it = iterator
    while(it.hasNext) {
      val v = it.next
      if (v == a) return Opt(idx)
      idx += 1
    }
    Opt.empty[SafeLong]
  }
  def map[B](f: A => B): BigIndexedSeq[B] = new MappedBigIndexedSeq[A, B](self, f)
  override def toIndexedSeq: scala.collection.immutable.IndexedSeq[A] = {
    require(length.isValidInt)
    new scala.collection.immutable.IndexedSeq[A] {
      def apply(idx: Int): A = self.apply(SafeLong(idx))
      def length: Int = self.length.toInt
    }
  }
}

final class MappedBigIndexedSeq[A, B](original: BigIndexedSeq[A], f: A => B) extends BigIndexedSeq[B] {
  def length = original.length
  def iterator = original.iterator.map(f)
  def apply(idx: SafeLong): B = f(original(idx))
  override def map[C](g: B => C): BigIndexedSeq[C] = new MappedBigIndexedSeq(original, f andThen g)
}
