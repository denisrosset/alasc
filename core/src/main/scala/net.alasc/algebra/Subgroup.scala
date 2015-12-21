package net.alasc
package algebra

import scala.util.Random
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group, PartialOrder}
import spire.syntax.eq._

import net.alasc.math.Grp
import net.alasc.syntax.permutationAction._
import net.alasc.util._

trait Subgroup[S, G] extends PartialOrder[S] { sg =>

  implicit def finiteGroup: FiniteGroup[G]
  implicit def equality: Eq[G]

  /** Tests if two subgroups are equivalent. */
  override def eqv(x: S, y: S): Boolean = (order(x) == order(y)) && lteqv(x, y)
  override def lteqv(x: S, y: S): Boolean = generators(x).forall(contains(y, _))
  override def gteqv(x: S, y: S): Boolean = generators(y).forall(contains(x, _))
  override def lt(x: S, y: S): Boolean = lteqv(x, y) && (order(x) < order(y))
  override def gt(x: S, y: S): Boolean = gteqv(x, y) && (order(x) > order(y))

  def hasSubgroup(x: S, y: S): Boolean = gteqv(x, y)
  def hasProperSubgroup(x: S, y: S): Boolean = gt(x, y)
  def isSubgroupOf(x: S, y: S): Boolean = lteqv(x, y)
  def isProperSubgroupOf(x: S, y: S): Boolean = lt(x, y)

  def partialCompare(x: S, y: S): Double = {
    val c = order(x).compare(order(y))
    if (c < 0) {
      if (lteqv(x, y)) -1.0 else Double.NaN
    } else if (c > 0) {
      if (gteqv(x, y)) 1.0 else Double.NaN
    } else { // c == 0
      if (lteqv(x, y)) 0.0 else Double.NaN
    }
  }

  /** Iterator through the subgroup elements. */
  def iterator(s: S): Iterator[G]
  /** Iterable of the subgroup generators. */
  def generators(s: S): Iterable[G]
  /** Order of the subgroup `s`. */
  def order(s: S): BigInt
  /** Generates a random element of the group. */ 
  def randomElement(s: S, gen: Random): G
  /** Tests if the element `g` is contained inside `s`. */
  def contains(s: S, g: G): Boolean
  /** Returns the minimal domain element which is permuted by `s`, or `NNNone` if
    * `s` is the trivial group = ().
    */
  def supportMin(s: S)(implicit ev: FaithfulPermutationAction[G]): NNOption = {
    var res: NNOption = NNNone
    generators(s).foreach { g =>
      val gMin = g.supportMin
      if (res.isEmpty)
        res = gMin
      else if (gMin.isDefined)
        res = NNSome(gMin.get.min(res.get))
    }
    res
  }
  /** Returns the maximal domain element which is permuted by `s`, or `NNNone` if
    * `s` is the trivial group = ().
    */
  def supportMax(s: S)(implicit ev: FaithfulPermutationAction[G]): NNOption = {
    var res: NNOption = NNNone
    generators(s).foreach { g =>
      val gMax = g.supportMax
      if (res.isEmpty) 
        res = gMax
      else if (gMax.isDefined)
        res = NNSome(gMax.get.max(res.get))
    }
    res
  }
  def supportAny(s: S)(implicit ev: FaithfulPermutationAction[G]): NNOption = {
    generators(s).foreach { g =>
      val sup = g.supportAny
      if (sup.nonEmpty)
        return sup
    }
    NNNone
  }
  def toGrp(subgroup: S)(implicit classTag: ClassTag[G], representations: Representations[G]): Grp[G] = Grp.fromSubgroup[S, G](subgroup)(classTag, representations, sg)
}
