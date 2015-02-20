package net.alasc
package math

import scala.util.Random

import spire.util.Opt

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
  givenOrder: Opt[BigInt] = Opt.empty[BigInt],
  givenRandomElement: Opt[Function1[Random, G]] = Opt.empty[Function1[Random, G]],
  givenRepresentation: Opt[Representation[G]] = Opt.empty[Representation[G]])(
  implicit val algorithms: BasicAlgorithms[G], val representations: Representations[G]) extends GrpLazyBase[G] { lhs =>

  private[this] var computedRepresentation: Opt[Representation[G]] = givenRepresentation
  /** Forces the computation of the representation.
    *
    * @param givenRepresentation   Representation to be used to avoid computation
    */
  protected def computeRepresentation(givenRepresentation: Opt[Representation[G]] = Opt.empty[Representation[G]]): Representation[G] =
    this.synchronized {
      if (computedRepresentation.isEmpty) {
        if (givenRepresentation.nonEmpty)
          computedRepresentation = givenRepresentation
        else
          computedRepresentation = Opt(representations.get(generators))
      }
      computedRepresentation.get
    }

  private[this] var computedChain: Opt[Chain[G]] = Opt.empty[Chain[G]]
  /** Forces the computation of the group chain.
    *
    * @param givenRepresentation   Representation to be used; the one given during `GrpLazy` has priority.
    */
  protected def computeChain(givenRepresentation: Opt[Representation[G]] = Opt.empty[Representation[G]]): Chain[G] =
    this.synchronized {
      val baseGuide = BaseGuide.empty
      if (computedChain.isEmpty) {
        computedChain = Opt(givenOrder match {
          case Opt(order) => givenRandomElement match {
            case Opt(randomElement) => algorithms.chainWithBase(generators, randomElement, order, baseGuide, representation.action)
            case _ => algorithms.chainWithBase(generators, order, baseGuide, representation.action)
          }
          case _ => algorithms.chainWithBase(generators, baseGuide, representation.action)
        })
      }
      computedChain.get
    }

  def representation: Representation[G] = computedRepresentation match {
    case Opt(r) => r
    case _ => computeRepresentation()
  }

  def chainIfComputed: Opt[Chain[G]] = computedChain
  def representationIfComputed: Opt[Representation[G]] = computedRepresentation

  def action: FaithfulPermutationAction[G] = representation.action

  def order: BigInt = givenOrder.getOrElse(chain.order)
  def isOrderComputed: Boolean = givenOrder.nonEmpty || computedChain.nonEmpty
  def orderIfComputed: Opt[BigInt] = if (isOrderComputed) Opt(order) else Opt.empty[BigInt]

  def randomElement(random: Random): G = givenRandomElement match {
    case Opt(f) => f(random)
    case _ => chain.randomElement(random)
  }

  def contains(g: G) = chain.contains(g)
}