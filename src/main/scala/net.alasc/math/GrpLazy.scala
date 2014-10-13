package net.alasc
package math

import scala.annotation.tailrec
import scala.util.Random

import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.partialOrder._

import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._
import bsgs._
import algorithms._
/**
  * @param givenOrder          Known order for the group, enabling the use of faster randomized algorithms.
  * @param givenRandomElement  Function that provides a random element of the group, for use with randomized algorithms.
  */
class GrpLazy[G](
  val generators: Iterable[G],
  givenOrder: RefOption[BigInt] = RefNone,
  givenRandomElement: RefOption[Function1[Random, G]] = RefNone)(
  implicit val algorithms: BasicAlgorithms[G], val representations: Representations[G]) extends GrpLazyBase[G] { lhs =>

  private[this] var computedRepresentation: RefOption[Representation[G]] = RefNone
  private[this] var computedChain: RefOption[Chain[G]] = RefNone

  protected def compute(givenRepresentation: RefOption[Representation[G]] = RefNone,
    givenBaseGuide: RefOption[BaseGuide] = RefNone): Unit =
    this.synchronized {
      if (computedRepresentation.nonEmpty) {
        assert(computedChain.nonEmpty)
      } else {
        assert(computedChain.isEmpty)
        val representation = givenRepresentation.getOrElse(representations.get(generators))
        val baseGuide = givenBaseGuide.getOrElse(BaseGuide.empty)
        computedChain = RefSome(givenOrder match {
          case RefOption(order) => givenRandomElement match {
            case RefOption(randomElement) => algorithms.chainWithBase(generators, randomElement, order, baseGuide, representation.action)
            case _ => algorithms.chainWithBase(generators, order, baseGuide, representation.action)
          }
          case _ => algorithms.chainWithBase(generators, baseGuide, representation.action)
        })
        computedRepresentation = RefSome(representation)
      }
    }

  def representation: Representation[G] = computedRepresentation match {
    case RefOption(r) => r
    case _ => compute()
      representation
  }

  def isChainComputed = computedChain.nonEmpty
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
