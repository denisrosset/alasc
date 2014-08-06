package net.alasc.algebra

import spire.algebra.Group
import scala.util.Random
import spire.syntax.eq._
import net.alasc.syntax.permutation._

trait Subgroup[S, G] {
  implicit val algebra: FiniteGroup[G]
  /** Iterable through the subgroup elements. */
  def elements(s: S): Iterable[G]
  /** Sequence of the subgroup generators. */
  def generators(s: S): Seq[G]
  /** Order of the subgroup `s`. */
  def order(s: S): BigInt
  /** Generates a random element of the group. */ 
  def random(s: S, gen: Random): G
  /** Tests if the element `g` is contained inside `s`. */
  def contains(s: S, g: G): Boolean = elements(s).exists(g === _)
  /** Returns the minimal domain element which is permuted by `s`.
    *
    * If the subgroup `s` is trival = (), returns -1.
    */
  def supportMin(s: S)(implicit ev: Permutation[G]): Int = generators(s).map(_.supportMin).reduceOption(_.min(_)).getOrElse(-1)
  /** Returns the maximal domain element which is permuted by `s`.
    * 
    * If the subgroup `s` is trival = (), returns -1.
    */
  def supportMax(s: S)(implicit ev: Permutation[G]): Int = ((-1) /: generators(s)) { case (sm, gen) => sm.max(gen.supportMax) }
}
