package net.alasc.algebra

import scala.collection.immutable.BitSet

import spire.algebra._
import spire.syntax.groupAction._

import net.alasc.math.Cycle
import net.alasc.syntax.permutation._
import net.alasc.util._

/** Type class for Permutation-like objects.
  * 
  * Combines Eq, Group, Signed and GroupAction[Int, _], along with
  * additional methods.
  * 
  * The standard action for the GroupAction[Int, P] is the right action.
  */
trait Permutation[P] extends FiniteGroup[P] with Signed[P] with GroupAction[Int, P] {
  self =>
  /** Returns a bit set of all integers k that are changed by the action of the permutation,
    * i.e. `S = { k | k <|+| p != k }`.
    */
  def support(p: P): BitSet
  /** Returns the maximal element in the support of `p`, or NNNone if the support is empty. */ 
  def supportMax(p: P): NNOption
  /** Returns the minimal element in the support of `p`, or NNNone if the support is empty. */
  def supportMin(p: P): NNOption
  /** Returns an arbitrary element in the support of `p` or NNNone if support empty. */
  def supportAny(p: P): NNOption = supportMax(p)
  /** Returns the value of the maximal support element support by this permutation type. */
  def supportMaxElement: Int
  /** Dummy overload for Signed, as one cannot change the sign of a permutation . */
  def abs(p: P): P = if (signum(p) == 1) p else sys.error(s"The permutation $p is odd.")
  def actl(p: P, k: Int) = actr(k, inverse(p))

  def to[Q](p: P)(implicit ev: BuildablePermutation[Q]): Q =
    ev.fromSupportAndImageFun(support(p), k => actr(k, p))
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

trait BuildablePermutation[P] extends Permutation[P] {
  def fromImages(images: Seq[Int]): P
  def fromSupportAndImageFun(support: BitSet, image: Int => Int): P
  def sorting[T: Order](seq: Seq[T]): P = {
    import spire.compat._
    fromImages(seq.zipWithIndex.sortBy(_._1).map(_._2))
  }
  def fromPermutation[Q: Permutation](q: Q): P =
    fromSupportAndImageFun(q.support, k => k <|+| q)
}
