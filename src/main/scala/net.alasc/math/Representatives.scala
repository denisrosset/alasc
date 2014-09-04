package net.alasc
package math

import spire.algebra.{Eq, GroupAction}
import spire.syntax.groupAction._

import net.alasc.algebra._

trait Representative[T, G] {
  def get: T
  def element: G
}

trait LexRepresentative[T, G] extends Representative[T, G] {
  def rank: BigInt
}

/** Describes the enumeration of representatives of sequence-like objects of type T under permutation by
  * a group of elements of type G.
  * 
  * `T` can thus be a sequence, a regular Java array, a linear algebra vector, and does not need to implement
  * any particular interface, because of the type class magic.
  */
trait RepresentativesIterable[T, G] {
  /** Permutation action of `G` on sequence-like `T`. */
  implicit def actionTG: GroupAction[T, G]
  /** Returns the partition given by equivalent elements in `t`. */
  def partition(t: T): Partition
  /** Returns a representation of G specific to an instance `t` of `T`. */
  def representation(t: T): Representation[G]

  def of(t: T, g: Grp[G]): coll.Iterable[Representative[T, G]] = new coll.Iterable[Representative[T, G]] {
    val symG = g.fixingPartitionW(partition(t), representation(t))
    val cosets = symG \ g
    def size = coll.BigIntSize(cosets.size)
    def iterator = cosets.iterator.map { coset => new Representative[T, G] {
      val element = coset.g
      def get = t <|+| element
    } }
    def foreach[U](f: Representative[T, G] => U): Unit = iterator.foreach(f)
  }
}

object RepresentativesIterable {
  def apply[A: Eq] = new RepresentativesIterable[Seq[A], Perm] {
    implicit val actionTG: GroupAction[Seq[A], Perm] = net.alasc.std.seq.SeqPermutationAction[Seq, A, Perm]
    def partition(t: Seq[A]) = Partition.fromSeqEq(t)
    def representation(t: Seq[A]) = Perm.Representations.forSize(t.size)
  }
}
