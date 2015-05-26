package net.alasc
package math
package enum

import scala.reflect.ClassTag

import spire.algebra.partial.RightPartialAction

import net.alasc.algebra._

/** Describes a sequence `T` that can be permuted by a group element `G`. */
trait Permutable[T, G] {
  /** Action of the group on the sequence. */ 
  implicit def action: RightPartialAction[T, G]
  /** Group representation for a sequence instance. */
  def representation(t: T): Representation[G]
  /** Symmetry subgroup of `grp` for the sequence `t`. */
  def symmetryGroup(t: T, grp: Grp[G])(implicit T: Enumerable[T]): Grp[G] = {
    import grp.algebra
    grp.fixingPartition(T.partition(t), representation(t))
  }
}

object Permutable {
  implicit def seq[A, G: FiniteGroup: FaithfulPermutationAction: PermutationRepresentations]: Permutable[Seq[A], G] = new PermutableSeq[A, G]
  implicit def array[A: ClassTag, G: FaithfulPermutationAction: PermutationRepresentations]: Permutable[Array[A], G] = new PermutableArray[A, G]
//  implicit def setInt[S <: Set[Int], G: FaithfulPermutationAction: PermutationRepresentations]: 
}
/*
final class PermutableSetInt[S <: Set[Int], G: FiniteGroup: FaithfulPermutationAction: PermutationRepresentations] extends Permutable[S, G] {
  def action = net.alasc.std.bitset.SetPermutationAction[
}*/

final class PermutableSeq[A, G: FiniteGroup: FaithfulPermutationAction: PermutationRepresentations] extends Permutable[Seq[A], G] {
  def action = net.alasc.std.seq.SeqPermutationAction[Seq, A, G]
  def representation(t: Seq[A]) = PermutationRepresentations[G].forSize(t.size)
}

final class PermutableArray[A: ClassTag, G: FaithfulPermutationAction: PermutationRepresentations] extends Permutable[Array[A], G]{
  def action = net.alasc.std.array.ArrayPermutationAction[A, G]
  def representation(t: Array[A]) = PermutationRepresentations[G].forSize(t.length)
}
