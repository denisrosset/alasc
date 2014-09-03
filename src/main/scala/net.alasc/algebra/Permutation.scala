package net.alasc.algebra

import scala.{ specialized => spec }
import scala.annotation.tailrec
import scala.collection.BitSet
import scala.collection.mutable.{ BitSet => MutableBitSet }
import spire.algebra._
import spire.syntax.groupAction._

import net.alasc.math.Cycle
import net.alasc.util._

/** Type class for Permutation-like objects.
  * 
  * Combines Eq, Group, Signed and GroupAction[Int, _], along with
  * additional methods.
  * 
  * The standard action for the GroupAction[Int, P] is the right action.
  */
trait Permutation[P] extends FiniteGroup[P] with FaithfulPermutationAction[P] {
  self =>
  def actl(p: P, k: Int) = actr(k, inverse(p))

  def fromImages(images: Seq[Int]): P
  def fromSupportAndImageFun(support: BitSet, image: Int => Int): P
  def sorting[T: Order](seq: Seq[T]): P = {
    import spire.compat._
    fromImages(seq.zipWithIndex.sortBy(_._1).map(_._2))
  }

  def from[Q](q: Q)(implicit evQ: FaithfulPermutationAction[Q]): P =
    fromSupportAndImageFun(evQ.support(q), k => evQ.actr(k, q))

  def compatibleWith(p: P) = true
}

object Permutation {
  def apply[P: Permutation] = implicitly[Permutation[P]]
}

trait ShiftablePermutation[P] extends Permutation[P] {
  /** Adds `n` to the domain elements acted on by `p`.
    *
    * Returns `p1` such that `k <|+| p1 = k` for `k < n`, and otherwise `k <|+| p1 = ((k - n) <|+| p) + n`.
    */
  def plus(p: P, n: Int): P
  /** Subtracts `n` to the domain elements acted on by `p`, assuming that
    * k <|+| p == k for k < n.
    * 
    * Returns `p1` such that `k <|+| p1 = ((k + n) <|+| p) - n`.
    */
  def minus(p: P, n: Int): P
}
