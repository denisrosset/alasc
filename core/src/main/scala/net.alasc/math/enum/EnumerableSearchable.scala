package net.alasc
package math
package enum

import spire.NoImplicit
import spire.algebra.Order
import spire.util.Opt

/** Describes a type of sequences where the elements of the two sequences can be tested for
  * equality.
  */
trait EnumerableSearchable[T] extends Enumerable[T] {
  /** Returns a partition map of `from`, where the indices of equal elements are grouped, pointing
    * to indices of equal elements in the sequence `to`.
    */
  def commonPartitions(from: T, to: T): Opt[PartitionMap[Set[Int]]]
}

object EnumerableSearchable {

  implicit def seqWithHashCode[A: HashCodeEquals]: EnumerableSearchable[Seq[A]] = new EnumerableSequenceSeqWithHashCode[A]

  implicit def arrayWithHashCode[A: HashCodeEquals]: EnumerableSearchable[Array[A]] = new EnumerableSequenceArrayWithHashCode[A]

  implicit def seqWithOrder[A: Order](implicit ev: NoImplicit[HashCodeEquals[A]]): EnumerableSearchable[Seq[A]] = new EnumerableOrderedSeq[A]
  
}
