package net.alasc.algebra

import scala.{ specialized => spec }
import scala.annotation.tailrec
import scala.collection.mutable

import spire.algebra._
import spire.syntax.action._

import net.alasc.math.Cycle
import net.alasc.util._

trait PermutationAction[G] extends Action[Int, G] with Signed[G] { self =>
  def abs(g: G) = sys.error("Abs is not supported")

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

  def orbit(g: G, i: Int): Set[Int] = {
    val mut = mutable.BitSet(i)
    @tailrec def rec(k: Int): Unit =
      if (k != i) {
        mut += k
        rec(actr(k, g))
      }
    rec(actr(i, g))
    mut.toImmutable
  }

  // TODO: remove, as `to` is sufficient
  def images(g: G, n: Int): IndexedSeq[Int] = new IndexedSeq[Int] {
    require(supportMax(g).getOrElseFast(-1) < n)
    def length = n
    def apply(idx: Int) = actr(idx, g)
  }

  def signum(g: G) = {
    // optimized for dense permutation on non-huge domains
    val toCheck = mutable.BitSet.empty ++= support(g)
    var parity = 1
    while (!toCheck.isEmpty) {
      val start = toCheck.head
      val o = orbit(g, start)
      if (o.size % 2 == 0) parity *= -1
      toCheck --= o
    }
    parity
  }

  def to[P](g: G)(implicit evP: Permutation[P]): P =
    evP.fromSupportAndImageFun(support(g), k => actr(k, g))

  def on[H](f: H => G): PermutationAction[H] = new MappedPermutationAction(self)(f)
}

class MappedPermutationAction[G, H](action: PermutationAction[H])(f: G => H) extends PermutationAction[G] {
  def actr(p: Int, g: G): Int = action.actr(p, f(g))
  def actl(g: G, p: Int): Int = action.actl(f(g), p)
  def support(g: G): Set[Int] = action.support(f(g))
  def supportMax(g: G): NNOption = action.supportMax(f(g))
  def supportMin(g: G): NNOption = action.supportMin(f(g))
  def supportMaxElement: Int = action.supportMaxElement
}

object PermutationAction {
  def apply[P](implicit P: PermutationAction[P]): PermutationAction[P] = P
}


trait FaithfulPermutationAction[G] extends PermutationAction[G]

object FaithfulPermutationAction {
  def apply[P](implicit P: FaithfulPermutationAction[P]): FaithfulPermutationAction[P] = P
}

