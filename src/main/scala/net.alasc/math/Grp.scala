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

/** User-friendly representation of a group internally using a base and strong generating set data structure.
  *
  * TODO: make thread-safe
  * 
  * Can be constructed from any finite group with a faithful permutation action.
  */
sealed abstract class Grp[G] { lhs =>
  override def toString = generators.mkString("Grp(", ", ", ")") //+ (if (knownOrder.nonEmpty || knownChain.nonEmpty) s" of order ${order}" else "")

  /** Set of algorithms used in the computations. */
  def algorithms: BasicAlgorithms[G]
  /** Generators of the group. Must not contain the identity. */
  def generators: Iterable[G]
  /** Representation provider for elements of G, used only if needed. */
  implicit def representations: Representations[G]
  /** Finite group operations on type G. */

  implicit def algebra: FiniteGroup[G]

  def order: BigInt
  def orderIfComputed: RefOption[BigInt]
  def chainIfComputed: RefOption[Chain[G]]
  def chain: Chain[G]
  def chain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Chain[G]
  def representation: Representation[G]
  def representationIfComputed: RefOption[Representation[G]]
  def randomElement(random: Random): G
  def contains(g: G): Boolean

  implicit def lattice = Grp.lattice[G](algebra, representations, algorithms)
}

class GrpChain[G](val algorithms: BasicAlgorithms[G], val generators: Iterable[G], val representation: Representation[G], val chain: Chain[G])(implicit val algebra: FiniteGroup[G], val representations: Representations[G]) extends Grp[G] { lhs =>
  chain match {
    case node: Node[G] => require(node.action == representation.action)
    case _: Term[G] =>
  }
  def chain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty) =
    algorithms.chainWithBase(chain, baseGuide, representation.action)
  def chainIfComputed = RefSome(chain)
  def order = chain.order
  def orderIfComputed = RefSome(order)
  def randomElement(random: Random) = chain.randomElement(random)
  def representationIfComputed = RefSome(representation)
  def contains(g: G) = chain.contains(g)
}

/** Represents a conjugated group from an original group G (represented by `originalChain`) and an InversePair(g, gInv).
  * The represented group is `H = gInv G g`.
  */
case class GrpConjugated[G](algorithms: BasicAlgorithms[G], originalGenerators: Iterable[G], representation: Representation[G], originalChain: Chain[G], conjugatedBy: InversePair[G])(implicit val algebra: FiniteGroup[G], val representations: Representations[G]) extends Grp[G] { lhs =>
  import conjugatedBy.{g, gInv}
  originalChain match {
    case node: Node[G] => require(node.action == representation.action)
    case _: Term[G] =>
  }
  def representationIfComputed = RefSome(representation)
  def chainIfComputed = RefSome(chain)
  def chain = originalChain match {
    case node: Node[G] =>
      implicit def action = representation.action
      import algorithms.nodeBuilder
      val mut = algorithms.mutableChain(node)(node.action)
      mut.conjugate(conjugatedBy)
      mut.toChain
    case term: Term[G] => term
  }
  def chain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty) = originalChain match {
    case node: Node[G] =>
      if (node.action == representation.action) {
        implicit def action = representation.action
        import algorithms.nodeBuilder
        val mut = algorithms.mutableChain(node)(node.action)
        mut.conjugate(conjugatedBy)
        algorithms.changeBaseSameAction(mut, baseGuide)
        mut.toChain
      } else
        algorithms.chainWithBase(generators, randomElement(_), order, baseGuide, representation.action)
    case term: Term[G] => term
  }
  def order = originalChain.order
  def orderIfComputed = RefSome(order)
  def randomElement(random: Random) = {
    val h = originalChain.randomElement(random)
    h.conjBy(conjugatedBy)
  }
  // `h in gInv G g` if and only if `g h gInv in G`.
  def contains(h: G) = originalChain.contains(g |+| h |+| gInv)
  def generators = originalChain.generators.map(h => gInv |+| h |+| g)
}

abstract class GrpLazyBase[G] extends Grp[G] {
  def isChainComputed: Boolean
  def isOrderComputed: Boolean
  def orderIfComputed: RefOption[BigInt]
  def chainIfComputed: RefOption[Chain[G]]

  protected def compute(givenRepresentation: RefOption[Representation[G]] = RefNone, givenBaseGuide: RefOption[BaseGuide] = RefNone): Unit

  def chain: Chain[G] = chain(representation)

  def chain(representationToUse: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Chain[G] =
    chainIfComputed match {
      case RefOption(node: Node[G]) => algorithms.chainWithBase(node, baseGuide, representationToUse.action)
      case RefOption(term: Term[G]) => term
      case _ =>
        compute(RefSome(representationToUse), RefSome(baseGuide))
        chain(representationToUse, baseGuide)
    }
}


object Grp {
  def lattice[G](implicit algebra: FiniteGroup[G], representations: Representations[G], algorithms: BasicAlgorithms[G]): BoundedBelowLattice[Grp[G]] = new GrpLattice[G]
  def trivial[G](implicit algebra: FiniteGroup[G], rp: Representations[G]) = apply[G]()
  def defaultAlgorithms[G](implicit algebra: FiniteGroup[G]) = BasicAlgorithms.randomized(Random)

  def fromChain[G](chain: Chain[G], representation: Representation[G])(
    implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] = {
    chain match {
      case node: Node[G] => require(representation.action == node.action)
      case _: Term[G] =>
    }
    new GrpChain[G](defaultAlgorithms[G], chain.generators, representation, chain)
  }

  def fromChain[G](chain: Chain[G])(
    implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] = {
    val representation = rp.get(chain.generators)
    chain match {
      case node: Node[G] if representation.action != node.action =>
        new GrpLazy(defaultAlgorithms[G], chain.generators, RefSome(chain.order), RefSome(chain.randomElement(_)))
      case _ => fromChain(chain, representation)
    }
  }

  def fromGenerators[G](generators: Iterable[G])(
    implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] =
    new GrpLazy[G](defaultAlgorithms[G], generators)

  def apply[G](generators: G*)(implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] =
    new GrpLazy[G](defaultAlgorithms[G], generators)

  def fromGeneratorsAndOrder[G](generators: Iterable[G], order: BigInt)(
    implicit algebra: FiniteGroup[G], rp: Representations[G]): Grp[G] =
    new GrpLazy[G](defaultAlgorithms[G], generators, givenOrder = RefSome(order))

  def fromSubgroup[S, G](subgroup: S)(
    implicit algebra: FiniteGroup[G], sg: Subgroup[S, G], rp: Representations[G]): Grp[G] =
    new GrpLazy[G](defaultAlgorithms[G], subgroup.generators,
      givenOrder = RefSome(subgroup.order), givenRandomElement = RefSome(subgroup.randomElement(_)))

  implicit def GrpSubgroup[G](implicit algebra: FiniteGroup[G]): Subgroup[Grp[G], G] = new GrpSubgroup[G]
}

class GrpSubgroup[G](implicit val algebra: FiniteGroup[G]) extends Subgroup[Grp[G], G] {
  def iterator(grp: Grp[G]) = grp.chain.iterator
  def generators(grp: Grp[G]) = grp.generators
  def order(grp: Grp[G]) = grp.order
  def randomElement(grp: Grp[G], random: Random) = grp.randomElement(random)
  override def contains(grp: Grp[G], g: G) = grp.contains(g)
  override def toGrp(grp: Grp[G])(implicit representations: Representations[G]): Grp[G] = grp
}
