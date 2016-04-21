package net.alasc.algebra

import scala.annotation.tailrec

import spire.algebra._
import spire.std.int._

import metal.syntax._

import net.alasc.util._

/** Permutation action: a group action on non-negative integers.
  *
  * The domain on which the permutation acts starts at 0.
  *
  * The standard action, whose speed should be optimized, is the right action.
  *
  */
trait PermutationAction[G] extends Action[Int, G] { self =>

  /** Tests if the point `i` is in the support of `g`. */
  def movesPoint(g: G, i: Int): Boolean = actr(i, g) != i

  /** Number of non-negative integers moved by the permutation. */
  def nMovedPoints(g: G): Int

  /** Returns a bit set of all non-negative integers k that are changed by the action of the permutation,
    * i.e. `S = { k | k <|+| g != k }`.
    */
  def movedPoints(g: G): Set[Int]

  /** Returns the maximal element in the support of ` g`, or NNNone if the support is empty. */
  def largestMovedPoint(g: G): NNOption

  /** Returns the minimal element in the support of `g`, or NNNone if the support is empty. */
  def smallestMovedPoint(g: G): NNOption

  /** Returns an arbitrary element in the support of `g` or NNNone if support empty. */
  def findMovedPoint(g: G): NNOption = largestMovedPoint(g)

  /** Returns an upper bound on the maximal element in the support of any element of `G`. */
  def movedPointsUpperBound: Int

  /** Returns the sign of the permutation `g`. */
  def signPerm(g: G): Int = {
    val rest = metal.mutable.BitSet.fromIterable(movedPoints(g))
    var sign = 1
    while (rest.nonEmpty) {
      val h = rest.min
      var iter = h
      var n = 0
      while (n == 0 || iter != h) {
        rest -= iter
        n += 1
        iter = actr(iter, g)
      }
      if (n % 2 == 0)
        sign = -sign
    }
    sign
  }

  /** Returns the cycle structure of the permutation `g`. */
  def cycleStructure(g: G): Map[Int, Int] = {
    val rest = metal.mutable.BitSet.fromIterable(movedPoints(g))
    val cs = metal.mutable.HashMap.empty[Int, Int]
    while (rest.nonEmpty) {
      val h = rest.min
      var iter = h
      var n = 0
      while (n == 0 || iter != h) {
        rest -= iter
        n += 1
        iter = actr(iter, g)
      }
      cs(n) = cs.getOrElse(n, 0) + 1
    }
    cs.toScala
  }

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
    require(largestMovedPoint(g).getOrElseFast(-1) < n)
    def length = n
    def apply(idx: Int) = actr(idx, g)
  }

  def toPermutation[P](g: G)(implicit evP: PermutationBuilder[P]): P =
    evP.fromSupportAndImageFun(movedPoints(g), k => actr(k, g))

}

object PermutationAction {

  def apply[G](implicit G: PermutationAction[G]): PermutationAction[G] = G

  /** Return the smallest element of the domain moved by the given generators, or [[NNNone]]. */
  def smallestMovedPoint[G](generators: Iterable[G])(implicit action: PermutationAction[G]): NNOption = {
    var mn = Int.MaxValue
    var moved = false
    generators.foreach { g =>
      action.smallestMovedPoint(g) match {
        case NNOption(i) =>
          mn = spire.math.min(mn, i)
          moved = true
        case _ =>
      }
    }
    if (moved) NNOption(mn) else NNNone
  }

  /** Return the largest element of the domain moved by the given generators, or [[NNNone]]. */
  def largestMovedPoint[G](generators: Iterable[G])(implicit action: PermutationAction[G]): NNOption = {
    var mx = -1
    generators.foreach { g =>
      mx = spire.math.max(action.largestMovedPoint(g).getOrElseFast(-1), mx)
    }
    if (mx >= 0) NNOption(mx) else NNNone
  }

}
