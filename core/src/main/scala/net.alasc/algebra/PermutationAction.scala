package net.alasc.algebra

import scala.annotation.tailrec

import spire.algebra._

import metal.syntax._

import net.alasc.util._

import metal.syntax._

trait PermutationAction[G] extends Action[Int, G] { self =>

  /** Tests if the point `i` is in the support of `g`. */
  def inSupport(g: G, i: Int): Boolean = actr(i, g) != i

  /** Returns a bit set of all integers k that are changed by the action of the permutation,
    * i.e. `S = { k | k <|+| g != k }`.
    */
  def support(g: G): Set[Int]

  /** Returns the maximal element in the support of ` g`, or NNNone if the support is empty. */ 
  def supportMax(g: G): NNOption

  /** Returns the minimal element in the support of `g`, or NNNone if the support is empty. */
  def supportMin(g: G): NNOption

  /** Returns an arbitrary element in the support of `g` or NNNone if support empty. */
  def supportAny(g: G): NNOption = supportMax(g)

  /** Returns an upper bound on the maximal element in the support of any element of `G`. */
  def supportMaxElement: Int

  /** Returns the orbit of the domain element `i` under the action of `g`. */
  def orbit(g: G, i: Int): Set[Int] = {
    val mut = metal.mutable.BitSet(i)
    @tailrec def rec(k: Int): Unit =
      if (k != i) {
        mut += k
        rec(actr(k, g))
      }
    rec(actr(i, g))
    mut.toScala
  }

  /** Returns the images of `g` on the domain (0 until n).
    *
    * Requires that `n > supportMax(g)`.*/
  def images(g: G, n: Int): IndexedSeq[Int] = new IndexedSeq[Int] {
    require(supportMax(g).getOrElseFast(-1) < n)
    def length = n
    def apply(idx: Int) = actr(idx, g)
  }

  def toPermutation[P](g: G)(implicit evP: PermutationBuilder[P]): P =
    evP.fromSupportAndImageFun(support(g), k => actr(k, g))

}

object PermutationAction {

  def apply[P](implicit P: PermutationAction[P]): PermutationAction[P] = P

}

trait FaithfulPermutationAction[G] extends PermutationAction[G]

object FaithfulPermutationAction {

  def apply[P](implicit P: FaithfulPermutationAction[P]): FaithfulPermutationAction[P] = P

}
