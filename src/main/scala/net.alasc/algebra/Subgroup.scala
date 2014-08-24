package net.alasc.algebra

import scala.util.Random

import spire.algebra.Group
import spire.syntax.eq._

import net.alasc.math.Grp
import net.alasc.syntax.permutationAction._
import net.alasc.util._

trait Subgroup[S, G] { sg =>
  implicit val algebra: FiniteGroup[G]
  /** Iterable through the subgroup elements. */
  def elements(s: S): Iterable[G]
  /** Iterable of the subgroup generators. */
  def generators(s: S): Iterable[G]
  /** Order of the subgroup `s`. */
  def order(s: S): BigInt
  /** Generates a random element of the group. */ 
  def randomElement(s: S, gen: Random): G
  /** Tests if the element `g` is contained inside `s`. */
  def contains(s: S, g: G): Boolean = elements(s).exists(g === _)
  /** Returns the minimal domain element which is permuted by `s`, or `NNNone` if
    * `s` is the trivial group = ().
    */
  def supportMin(s: S)(implicit ev: PermutationAction[G]): NNOption = {
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
  def supportMax(s: S)(implicit ev: PermutationAction[G]): NNOption = {
    var res: NNOption = NNNone
    generators(s).foreach { g =>
      val gMin = g.supportMin
      if (res.isEmpty) 
        res = gMin
      else if (gMin.isDefined)
        res = NNSome(gMin.get.max(res.get))
    }
    res
  }
  def toGrp(subgroup: S)(implicit action: PermutationAction[G]): Grp[G] = Grp.fromSubgroup[S, G](subgroup)(algebra, sg, action)
}
