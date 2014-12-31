package net.alasc
package math

import scala.util.Random

import spire.util.Nullbox

import net.alasc.algebra._
import net.alasc.math.bsgs._
import net.alasc.math.bsgs.algorithms._
import net.alasc.math.guide.BaseGuide
import net.alasc.syntax.all._

/**
  * @param generators          Generators of the group
  * @param givenOrder          Known order for the group, enabling the use of faster randomized algorithms.
  * @param givenRandomElement  Function that provides a random element of the group, for use with randomized algorithms.
  * @param givenRepresentation Representation to be used by default to compute the group chain.
  */
class GrpLazy[G](
  val generators: Iterable[G],
  givenOrder: Nullbox[BigInt] = Nullbox.empty[BigInt],
  givenRandomElement: Nullbox[Function1[Random, G]] = Nullbox.empty[Function1[Random, G]],
  givenRepresentation: Nullbox[Representation[G]] = Nullbox.empty[Representation[G]])(
  implicit val algorithms: BasicAlgorithms[G], val representations: Representations[G]) extends GrpLazyBase[G] { lhs =>

  private[this] var computedRepresentation: Nullbox[Representation[G]] = givenRepresentation
  /** Forces the computation of the representation.
    *
    * @param givenRepresentation   Representation to be used to avoid computation
    */
  protected def computeRepresentation(givenRepresentation: Nullbox[Representation[G]] = Nullbox.empty[Representation[G]]): Representation[G] =
    this.synchronized {
      if (computedRepresentation.isEmpty) {
        if (givenRepresentation.nonEmpty)
          computedRepresentation = givenRepresentation
        else
          computedRepresentation = Nullbox(representations.get(generators))
      }
      computedRepresentation.get
    }

  private[this] var computedChain: Nullbox[Chain[G]] = Nullbox.empty[Chain[G]]
  /** Forces the computation of the group chain.
    *
    * @param givenRepresentation   Representation to be used; the one given during `GrpLazy` has priority.
    */
  protected def computeChain(givenRepresentation: Nullbox[Representation[G]] = Nullbox.empty[Representation[G]]): Chain[G] =
    this.synchronized {
      val baseGuide = BaseGuide.empty
      if (computedChain.isEmpty) {
        computedChain = Nullbox(givenOrder match {
          case Nullbox(order) => givenRandomElement match {
            case Nullbox(randomElement) => algorithms.chainWithBase(generators, randomElement, order, baseGuide, representation.action)
            case _ => algorithms.chainWithBase(generators, order, baseGuide, representation.action)
          }
          case _ => algorithms.chainWithBase(generators, baseGuide, representation.action)
        })
      }
      computedChain.get
    }

  def representation: Representation[G] = computedRepresentation match {
    case Nullbox(r) => r
    case _ => computeRepresentation()
  }

  def chainIfComputed: Nullbox[Chain[G]] = computedChain
  def representationIfComputed: Nullbox[Representation[G]] = computedRepresentation

  def action: FaithfulPermutationAction[G] = representation.action

  def order: BigInt = givenOrder.getOrElse(chain.order)
  def isOrderComputed: Boolean = givenOrder.nonEmpty || computedChain.nonEmpty
  def orderIfComputed: Nullbox[BigInt] = if (isOrderComputed) Nullbox(order) else Nullbox.empty[BigInt]

  def randomElement(random: Random): G = givenRandomElement match {
    case Nullbox(f) => f(random)
    case _ => chain.randomElement(random)
  }

  def contains(g: G) = chain.contains(g)
}
