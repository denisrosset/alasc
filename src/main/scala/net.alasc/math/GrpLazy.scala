package net.alasc
package math

import scala.util.Random

import net.alasc.algebra._
import net.alasc.math.bsgs._
import net.alasc.math.bsgs.algorithms._
import net.alasc.math.guide.BaseGuide
import net.alasc.syntax.all._
import net.alasc.util._
/**
  * @param generators          Generators of the group
  * @param givenOrder          Known order for the group, enabling the use of faster randomized algorithms.
  * @param givenRandomElement  Function that provides a random element of the group, for use with randomized algorithms.
  * @param givenRepresentation Representation to be used by default to compute the group chain.
  */
class GrpLazy[G](
  val generators: Iterable[G],
  givenOrder: RefOption[BigInt] = RefNone,
  givenRandomElement: RefOption[Function1[Random, G]] = RefNone,
  givenRepresentation: RefOption[Representation[G]] = RefNone)(
  implicit val algorithms: BasicAlgorithms[G], val representations: Representations[G]) extends GrpLazyBase[G] { lhs =>

  private[this] var computedRepresentation: RefOption[Representation[G]] = givenRepresentation
  /** Forces the computation of the representation.
    *
    * @param givenRepresentation   Representation to be used to avoid computation
    */
  protected def computeRepresentation(givenRepresentation: RefOption[Representation[G]] = RefNone): Representation[G] =
    this.synchronized {
      if (computedRepresentation.isEmpty) {
        if (givenRepresentation.nonEmpty)
          computedRepresentation = givenRepresentation
        else
          computedRepresentation = RefSome(representations.get(generators))
      }
      computedRepresentation.get
    }

  private[this] var computedChain: RefOption[Chain[G]] = RefNone
  /** Forces the computation of the group chain.
    *
    * @param givenRepresentation   Representation to be used; the one given during `GrpLazy` has priority.
    */
  protected def computeChain(givenRepresentation: RefOption[Representation[G]] = RefNone): Chain[G] =
    this.synchronized {
      val baseGuide = BaseGuide.empty
      if (computedChain.isEmpty) {
        computedChain = RefSome(givenOrder match {
          case RefOption(order) => givenRandomElement match {
            case RefOption(randomElement) => algorithms.chainWithBase(generators, randomElement, order, baseGuide, representation.action)
            case _ => algorithms.chainWithBase(generators, order, baseGuide, representation.action)
          }
          case _ => algorithms.chainWithBase(generators, baseGuide, representation.action)
        })
      }
      computedChain.get
    }

  def representation: Representation[G] = computedRepresentation match {
    case RefOption(r) => r
    case _ => computeRepresentation()
  }

  def chainIfComputed: RefOption[Chain[G]] = computedChain
  def representationIfComputed: RefOption[Representation[G]] = computedRepresentation

  def action: FaithfulPermutationAction[G] = representation.action

  def order: BigInt = givenOrder.getOrElse(chain.order)
  def isOrderComputed: Boolean = givenOrder.nonEmpty || computedChain.nonEmpty
  def orderIfComputed: RefOption[BigInt] = if (isOrderComputed) RefSome(order) else RefNone

  def randomElement(random: Random): G = givenRandomElement match {
    case RefOption(f) => f(random)
    case _ => chain.randomElement(random)
  }

  def contains(g: G) = chain.contains(g)
}
