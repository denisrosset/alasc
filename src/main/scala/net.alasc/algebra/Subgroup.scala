package net.alasc
package algebra

import scala.util.Random

import spire.algebra.{Group, Eq}
import spire.syntax.eq._

import net.alasc.math.Grp
import net.alasc.syntax.permutationAction._
import net.alasc.util._

trait Subgroup[S, G] extends Eq[S] { sg =>
  implicit val algebra: FiniteGroup[G]
  /** Tests if two subgroups are equivalent. */
  def eqv(x: S, y: S) = generators(x).forall(contains(y, _)) && generators(y).forall(contains(x, _))
  /** Iterator through the subgroup elements. */
  def iterator(s: S): Iterator[G]
  /** Set of subgroup elements. */
  def elements(s: S): coll.Set[G] = new coll.Set[G] {
    def contains(g: G) = sg.contains(s, g)
    def foreach[U](f: G => U): Unit = sg.iterator(s).foreach(f)
    def iterator = sg.iterator(s)
    def size = coll.BigIntSize(sg.order(s))
  }
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
  def toGrp(subgroup: S)(implicit actions: FaithfulPermutationActions[G]): Grp[G] = Grp.fromSubgroup[S, G](subgroup)(algebra, sg, actions)
}
