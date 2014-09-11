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

sealed abstract class Grp[G] { lhs =>
  override def toString = generators.mkString("Grp(", ", ", ")") //+ (if (knownOrder.nonEmpty || knownChain.nonEmpty) s" of order ${order}" else "")

  /** Set of algorithms used in the computations. */
  def algorithms: BasicAlgorithms[G]
  /** Generators of the group. Must not contain the identity. */
  def generators: Iterable[G]
  /** Representation provider for elements of G, used only if needed;
    * givenRepresentation does not have to be compatible with these representations.
    */
  implicit def representations: Representations[G]
  implicit def algebra: FiniteGroup[G]

  def order: BigInt
  def orderIfComputed: RefOption[BigInt]
  def chain: Chain[G]
  def chainIfComputed: RefOption[Chain[G]]
  def chain(representationToUse: RefOption[Representation[G]] = RefNone, givenBase: Seq[Int] = Seq.empty): Chain[G]
  def representation: Representation[G]
  def representationIfComputed: RefOption[Representation[G]]
  def randomElement(random: Random): G
  def contains(g: G): Boolean

  implicit def lattice = Grp.lattice[G](algebra, representations, algorithms)
}

/*
class GrpChain[G](val algorithms: BasicAlgorithms[G], val generators: Iterable[G], val representation: Representation[G], val chain: Chain[G])(implicit val algebra: FiniteGroup[G], val representations: Representations[G]) extends GrpBase[G] { lhs =>

}
 */

/** User-friendly representation of a group internally using a base and strong generating set data structure.
  *
  * Can be constructed from any finite group with a faithful permutation action.
  * 
  * Note: `givenChain`, if provided, has to be accompanied with `givenRepresentation` such that
  * the action (if any) of `givenChain` is equal to `givenRepresentation.get.action`.
  * 
  * @param algorithms          Set of algorithms used in the computations.
  * @param generators          Generators of the group. Must not contain the identity.
  * @param givenRepresentation Force the use of a particular representation.
  * @param givenOrder          Known order for the group, enabling the use of faster randomized algorithms.
  * @param givenChain          Known chain for the group. When not empty, givenRepresentation has to provided,
  *                            and givenChain action (if givenChain is contains a node) has to be equal to
  *                            givenRepresentation.action.
  * @param givenRandomElement  Function that provides a random element of the group, for use with randomized algorithms.
  * @param algebra             Finite group operations on type G.
  * @param representations     Representation provider for elements of G, used only if needed; givenRepresentation 
  *                            does not have to be compatible with these representations.
  */
class GrpImpl[G](
  val algorithms: BasicAlgorithms[G],
  val generators: Iterable[G],
  givenRepresentation: RefOption[Representation[G]],
  givenOrder: RefOption[BigInt] = RefNone,
  givenChain: RefOption[Chain[G]] = RefNone,
  givenRandomElement: RefOption[Function1[Random, G]] = RefNone)(
  implicit val algebra: FiniteGroup[G], val representations: Representations[G]) extends Grp[G] { lhs =>

  // TODO conjugatedBy

  require(givenChain.fold(true)(_.isImmutable))

  private[this] var knownChain: RefOption[Chain[G]] = givenChain

  def isRepresentationKnown = knownRepresentation.nonEmpty
  protected def representationFromGenerators = representations.get(generators)
  private[this] var knownRepresentation: RefOption[Representation[G]] = givenRepresentation
  def representation: Representation[G] = knownRepresentation match {
    case RefOption(r) => r
    case _ =>
      val r = representationFromGenerators
      knownRepresentation = RefSome(r)
      r
  }
  def representation(advice: RefOption[Representation[G]]): Representation[G] = knownRepresentation match {
    case RefOption(r) => r
    case _ => advice match {
      case RefOption(r) =>
        knownRepresentation = advice
        r
      case _ => representation
    }
  }
  def representationIfComputed: RefOption[Representation[G]] =
    if (isRepresentationKnown) RefSome(representation) else RefNone

  def action: FaithfulPermutationAction[G] = knownChain match {
    case RefOption(node: Node[G]) =>
      assert(node.action == representation.action) // TODO remove
      node.action
    case _ => representation.action
  }

  def isChainComputed: Boolean = knownChain.nonEmpty

  protected def computeChain(base: Seq[Int])(implicit action: FaithfulPermutationAction[G]): Chain[G] = knownOrder match {
    case RefOption(order) => algorithms match {
      case ssr: SchreierSimsRandomized[G] => ssr.randomizedSchreierSims(randomElement(_), order, base).toChain
      case _ => algorithms.completeChainFromGeneratorsAndOrder(generators, order, base).toChain
    }
    case _ => algorithms.completeChainFromGenerators(generators, base).toChain
  }

  def chain: Chain[G] = chain()
  // TODO: instead of givenBase, provide a BaseGuide
  def chain(representationToUse: RefOption[Representation[G]] = RefNone, givenBase: Seq[Int] = Seq.empty): Chain[G] =
    knownChain match {
      case RefOption(node: Node[G]) => representationToUse match {
        case RefOption(rp) if (node.action != rp.action) => computeChain(givenBase)(rp.action)
        case _ => node // either representationToUse is empty, or the action is compatible
      }
      case RefOption(term: Term[G]) => term
      case _ => // knownChain is empty, we have to compute a chain
        if (knownRepresentation.nonEmpty) { // forced representation during Grp construction, use it to compute knownChain
          knownChain = RefSome(computeChain(givenBase)(representation.action))
          chain(representationToUse, givenBase) // but return one with the newly given representation
        } else {
          knownRepresentation = RefSome(representationToUse.getOrElse(representationFromGenerators))
          val c = computeChain(givenBase)(representation.action)
          knownChain = RefSome(c)
          c
        }
    }

  def isOrderComputed: Boolean = isChainComputed || knownOrder.nonEmpty
  private[this] var knownOrder: RefOption[BigInt] = givenOrder
  def order: BigInt = knownOrder.getOrElse {
    val o = chain.order
    knownOrder = RefSome(o)
    o
  }

  def orderIfComputed: RefOption[BigInt] = if (isOrderComputed) RefSome(order) else RefNone
  def chainIfComputed: RefOption[Chain[G]] = if (isChainComputed) RefSome(chain) else RefNone

  private[this] var knownRandomBag: RefOption[RandomBag[G]] = RefNone
  def randomElement(random: Random): G = givenRandomElement match {
    case RefOption(f) => f(random)
    case _ => knownChain match {
      case RefOption(chain) => chain.randomElement(random)
      case _ =>
        val rbag = knownRandomBag match {
          case RefOption(bag) => bag
          case _ =>
            val bag = RandomBag(generators, random)
            knownRandomBag = RefSome(bag)
            bag
        }
        rbag.randomElement(random)
    }
  }

  def contains(g: G) = chain.contains(g)
}

object Grp {
  def lattice[G](implicit algebra: FiniteGroup[G], representations: Representations[G], algorithms: BasicAlgorithms[G]): BoundedBelowLattice[Grp[G]] = new GrpLattice[G]
  def trivial[G](implicit algebra: FiniteGroup[G], rp: Representations[G]) = apply[G]()
  def defaultAlgorithms[G](implicit algebra: FiniteGroup[G]) = BasicAlgorithms.randomized(Random)

  def fromChain[G](chain: Chain[G], givenRepresentation: RefOption[Representation[G]] = RefNone)(
    implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] = {
    givenRepresentation.foreach { r => require(chain.generators.forall(r.represents(_))) } // TODO remove
    val representation = givenRepresentation.getOrElse(rp.get(chain.generators))
    chain match {
      case node: Node[G] if representation.action != node.action =>
        fromSubgroup[Chain[G], G](node, RefSome(representation))
      case _ => new GrpImpl[G](defaultAlgorithms[G], chain.generators, RefSome(representation), givenChain = RefSome(chain))
    }
  }

  def fromGenerators[G](generators: Iterable[G], givenRepresentation: RefOption[Representation[G]])(
    implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] =
    new GrpImpl[G](defaultAlgorithms[G], generators, givenRepresentation)

  def apply[G](generators: G*)(implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] =
    new GrpImpl[G](defaultAlgorithms[G], generators, RefNone)

  def fromGeneratorsAndOrder[G](generators: Iterable[G], order: BigInt, givenRepresentation: RefOption[Representation[G]] = RefNone)(
    implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] =
    new GrpImpl[G](defaultAlgorithms[G], generators, givenRepresentation, givenOrder = RefSome(order))

  def fromSubgroup[S, G](subgroup: S, givenRepresentation: RefOption[Representation[G]] = RefNone)(
    implicit algebra: FiniteGroup[G], sg: Subgroup[S, G], rp: Representations[G]): Grp[G] =
    new GrpImpl[G](defaultAlgorithms[G], subgroup.generators, givenRepresentation,
      givenOrder = RefSome(subgroup.order), givenRandomElement = RefSome(subgroup.randomElement(_)))

  implicit def GrpSubgroup[G](implicit algebra: FiniteGroup[G]): Subgroup[Grp[G], G] = new GrpSubgroup[G]
}

class GrpSubgroup[G](implicit val algebra: FiniteGroup[G]) extends Subgroup[Grp[G], G] {
  def iterator(grp: Grp[G]) = grp.chain.iterator
  def generators(grp: Grp[G]) = grp.generators
  def order(grp: Grp[G]) = grp.order
  def randomElement(grp: Grp[G], random: Random) = grp.randomElement(random)
  override def contains(grp: Grp[G], g: G) = grp.chain.contains(g)
  override def toGrp(grp: Grp[G])(implicit representations: Representations[G]): Grp[G] = grp
}
