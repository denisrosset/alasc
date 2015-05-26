package net.alasc
package math
package enum

import spire.NoImplicit
import spire.algebra.Order

/** Describes a sequence, whose elements can implicitly be compared for equality by computing
  * the partition of the domain corresponding to the sequence, where elements have the same
  * value.
  */
trait Enumerable[T] {
  /** Size of the sequence `t`. */
  def size(t: T): Int = partition(t).domain.size
  /** Partition of the domain corresponding to `t` where, for each partition of indices,
    * the elements corresponding to those indices have the same value.
    */
  def partition(t: T): Domain#Partition
}

object Enumerable {
  @inline final def apply[T](implicit T: Enumerable[T]): Enumerable[T] = T
  implicit def seqWithHashCode[A: HashCodeEquals]: Enumerable[Seq[A]] = new EnumerableSequenceSeqWithHashCode[A]
  implicit def arrayWithHashCode[A: HashCodeEquals]: Enumerable[Array[A]] = new EnumerableSequenceArrayWithHashCode[A]
  implicit def seqWithOrder[A: Order](implicit ev: NoImplicit[HashCodeEquals[A]]): Enumerable[Seq[A]] = new EnumerableOrderedSeq[A]
}
