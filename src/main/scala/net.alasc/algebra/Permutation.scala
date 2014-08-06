package net.alasc.algebra

import spire.algebra._
import scala.collection.immutable.BitSet
import net.alasc.math.Cycle
import net.alasc.syntax.permutation._
import spire.syntax.groupAction._

/** Type class for Permutation-like objects.
  * 
  * Combines Eq, Group, Signed and GroupAction[Int, _], along with
  * additional methods.
  */
trait Permutation[P] extends FiniteGroup[P] with Signed[P] with GroupAction[Int, P] {
  self =>
  /** Returns a bit set of all integers k that are changed by the action of the permutation,
    * i.e. `S = { k | k <|+| p != k }`.
    */
  def support(p: P): BitSet
  /** Returns the maximal element in the support of `p`, or -1 if the support is empty. */ 
  def supportMax(p: P): Int
  /** Returns the minimal element in the support of `p`, or -1 if the support is empty. */
  def supportMin(p: P): Int
  /** Returns an upper bound on the maximal support element (maximum Int.MaxValue). */
  def supportMaxElement: Int
  /** Dummy overload for Signed, as one cannot change the sign of a permutation . */
  def abs(p: P): P = if (signum(p) == 1) p else sys.error(s"The permutation $p is odd.")
}

trait BuildablePermutation[P] extends Permutation[P] {
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
  def fromImages(images: Seq[Int]): P
  def fromSupportAndImages(support: BitSet, image: Int => Int): P
  def sorting[T: Order](seq: Seq[T]): P = {
    import spire.compat._
    fromImages(seq.zipWithIndex.sortBy(_._1).map(_._2))
  }
  def fromPermutation[Q: Permutation](q: Q): P =
    fromSupportAndImages(q.support, k => k <|+| q)
}
