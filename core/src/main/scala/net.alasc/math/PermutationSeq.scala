package net.alasc.math

import spire.algebra.{GroupAction, Order}

import net.alasc.algebra._

trait PermutationResult[T, P] {
  implicit def get: T
  def permutation: P
}

/*class OrderedSeqPermutations[T, A, S, P](seq: T, subgroup: S)(
  implicit algebra: FiniteGroup[P], action: PermutationAction[P],
  seqAction: GroupAction[T, P], index: Index[T, A], sg: Subgroup[S, P],
  order: Order[A]) extends BigIndexedSeq[PermutationResult[T, P]] {

}
 */

/*class Permutations[T, A, S, P](seq: T, subgroup: S)(implicit algebra: FiniteGroup[P], action: PermutationAction[P], seqAction: GroupAction[T, P], sequence: Sequence[T, A], sg: Subgroup[S, P]) {
//  def ordered(implicit order: Order[A]) = new OrderedSeqPermutations(seq, subgroup)
}*/
