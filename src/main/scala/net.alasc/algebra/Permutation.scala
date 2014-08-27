package net.alasc.algebra

import scala.{ specialized => spec }
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable.{ BitSet => MutableBitSet }
import spire.algebra._
import spire.syntax.groupAction._

import net.alasc.math.Cycle
import net.alasc.util._

trait FaithfulAction[@spec(Int) P, G] extends GroupAction[P, G]

trait PermutationAction[P] extends FaithfulAction[Int, P] with Signed[P] {
  /** Returns a bit set of all integers k that are changed by the action of the permutation,
    * i.e. `S = { k | k <|+| p != k }`.
    */
  def support(p: P): BitSet
  /** Returns the maximal element in the support of ` p`, or NNNone if the support is empty. */ 
  def supportMax(p: P): NNOption
  /** Returns the minimal element in the support of `p`, or NNNone if the support is empty. */
  def supportMin(p: P): NNOption
  /** Returns an arbitrary element in the support of `p` or NNNone if support empty. */
  def supportAny(p: P): NNOption = supportMax(p)
  /** Returns the value of the maximal support element support by this permutation type. */
  def supportMaxElement: Int

  def images(p: P, n: Int): IndexedSeq[Int] = new IndexedSeq[Int] {
    require(supportMax(p).getOrElse(-1) < n)
    def length = n
    def apply(idx: Int) = actr(idx, p)
  }

  def signum(p: P) = {
    // optimized for dense permutation on non-huge domains
    val toCheck = MutableBitSet.empty ++= support(p)
    var parity = 0
    while (!toCheck.isEmpty) {
      val start = toCheck.head
      @tailrec def rec(k: Int): Unit = {
        toCheck -= k
        parity ^= 1
        val next = actr(k, p)
        if (next != start)
          rec(next)
      }
      rec(start)
    }
    if (parity == 0) 1 else -1
  }

  def to[Q](p: P)(implicit evQ: Permutation[Q]): Q =
    evQ.fromSupportAndImageFun(support(p), k => actr(k, p))
}

/** Type class for Permutation-like objects.
  * 
  * Combines Eq, Group, Signed and GroupAction[Int, _], along with
  * additional methods.
  * 
  * The standard action for the GroupAction[Int, P] is the right action.
  */
trait Permutation[P] extends FiniteGroup[P] with PermutationAction[P] {
  self =>
  def actl(p: P, k: Int) = actr(k, inverse(p))

  def fromImages(images: Seq[Int]): P
  def fromSupportAndImageFun(support: BitSet, image: Int => Int): P
  def sorting[T: Order](seq: Seq[T]): P = {
    import spire.compat._
    fromImages(seq.zipWithIndex.sortBy(_._1).map(_._2))
  }

  def from[Q](q: Q)(implicit evQ: PermutationAction[Q]): P =
    fromSupportAndImageFun(evQ.support(q), k => evQ.actr(k, q))
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
