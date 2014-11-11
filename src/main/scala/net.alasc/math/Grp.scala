package net.alasc
package math

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Random

import spire.syntax.group._
import spire.syntax.partialOrder._

import net.alasc.algebra._
import net.alasc.math.bsgs._
import net.alasc.math.bsgs.algorithms._
import net.alasc.math.guide.BaseGuide
import net.alasc.syntax.all._
import net.alasc.util._

/** User-friendly representation of a group internally using a base and strong generating set data structure.
  *
  * Can be constructed from any finite group with a faithful permutation action.
  * 
  * The generators must never contain the identity.
  */
sealed abstract class Grp[G] { lhs =>
  override def hashCode = sys.error("HashCode not defined for Grp")
  override def toString = generators.mkString("Grp(", ", ", ")") + (orderIfComputed match {
    case RefOption(o) => s" of order $o"
    case _ => ""
  })

  /** Set of algorithms used in the computations. */
  implicit def algorithms: BasicAlgorithms[G]

  /** Representation provider for elements of G, used only if needed. */
  implicit def representations: Representations[G]

  /** Generators of the group. Must not contain the identity. */
  def generators: Iterable[G]

  /** Finite group operations on type G. */
  implicit def algebra: FiniteGroup[G] = algorithms.algebra
  implicit def gClassTag: ClassTag[G] = algorithms.gClassTag

  override def equals(any: Any) = any match {
    case that: Grp[G] => this === that
    case _ => false
  }

  def isTrivial: Boolean = chainIfComputed match {
    case RefOption(c) => c.isTrivial
    case _ => generators.isEmpty
  }

  def order: BigInt
  def orderIfComputed: RefOption[BigInt]
  def chainIfComputed: RefOption[Chain[G]]
  def chain: Chain[G]
  def chain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Chain[G]
  def withComputedChain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Grp[G] =
    Grp.fromChain(chain(representation, baseGuide))
  def representation: Representation[G]
  def representationIfComputed: RefOption[Representation[G]]
  def randomElement(random: Random): G
  def contains(g: G): Boolean

  def conjBy(ip: InversePair[G]): Grp[G] = chainIfComputed match {
    case RefOption(chain) if representation.represents(ip.g) =>
      GrpConjugated(algorithms, generators, representation, chain, ip)
    case _ =>
      val conjRepresentation = representationIfComputed match {
        case RefOption(rep) if rep.represents(ip.g) => RefSome(rep)
        case _ => RefNone
      }
      orderIfComputed match {
        case RefOption(ord) => Grp.fromGeneratorsAndOrder(generators.map(_.conjBy(ip)), ord, conjRepresentation)
        case _ => Grp.fromGenerators(generators.map(_.conjBy(ip)), conjRepresentation)
      }
  }

  def reducedGenerators: Grp[G] = {
    implicit def builder = algorithms.nodeBuilder
    val mutableChain = algorithms.mutableChain(chain)(representation.action)
    mutableChain.makeFullyMutable()
    algorithms.removeRedundantGenerators(mutableChain)
    val newChain = mutableChain.toChain
    val newGenerators = if (generators.size < newChain.strongGeneratingSet.size) generators else newChain.strongGeneratingSet
    new GrpChain[G](newGenerators, representation, newChain)
  }

  implicit def lattice = Grp.lattice[G]
}

class GrpChain[G](val generators: Iterable[G], val representation: Representation[G], val chain: Chain[G])(implicit val algorithms: BasicAlgorithms[G], val representations: Representations[G]) extends Grp[G] { lhs =>
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
  * 
  * @note The `representation` must be able to represent `conjugatedBy.g`.
  */
case class GrpConjugated[G](algorithms: BasicAlgorithms[G], originalGenerators: Iterable[G], representation: Representation[G], originalChain: Chain[G], conjugatedBy: InversePair[G])(implicit val representations: Representations[G]) extends Grp[G] { lhs =>
  import conjugatedBy.{g, gInv}
  originalChain match {
    case node: Node[G] => require(node.action == representation.action)
    case _: Term[G] =>
  }
  def representationIfComputed = RefSome(representation)
  def chainIfComputed = RefSome(chain)
  override def isTrivial: Boolean = originalChain.isTrivial
  def chain = originalChain match {
    case node: Node[G] =>
      implicit def action = representation.action
      import algorithms.nodeBuilder
      val mut = algorithms.mutableChain(node)(node.action)
      mut.conjugate(conjugatedBy)
      mut.toChain
    case term: Term[G] => term
  }
  def chain(givenRepresentation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty) = originalChain match {
    case node: Node[G] =>
      if (node.action == givenRepresentation.action) {
        implicit def action = givenRepresentation.action
        import algorithms.nodeBuilder
        val mut = algorithms.mutableChain(node)
        mut.conjugate(conjugatedBy)
        algorithms.changeBaseSameAction(mut, baseGuide)
        mut.toChain
      } else
        algorithms.chainWithBase(generators, randomElement(_), order, baseGuide, givenRepresentation.action)
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
  override def conjBy(by: InversePair[G]): Grp[G] =
    if (representation.represents(by.g))
      new GrpConjugated(algorithms, originalGenerators, representation, originalChain, conjugatedBy |+| by)
    else
      super.conjBy(by)
}

abstract class GrpLazyBase[G] extends Grp[G] {
  def isChainComputed: Boolean
  def isOrderComputed: Boolean
  def orderIfComputed: RefOption[BigInt]
  def chainIfComputed: RefOption[Chain[G]]

  protected def computeChain(givenRepresentation: RefOption[Representation[G]] = RefNone): Chain[G]

  def chain: Chain[G] = chain(representation)

  def chain(representationToUse: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Chain[G] =
    chainIfComputed match {
      case RefOption(node: Node[G]) => algorithms.chainWithBase(node, baseGuide, representationToUse.action)
      case RefOption(term: Term[G]) => term
      case _ =>
        computeChain(RefSome(representationToUse))
        chain(representationToUse, baseGuide)
    }
}


object Grp {
  def lattice[G: ClassTag: FiniteGroup: Representations: BasicAlgorithms]: BoundedBelowLattice[Grp[G]] = new GrpLattice[G]
  def trivial[G: ClassTag: FiniteGroup: Representations] = apply[G]()
  implicit def defaultAlgorithms[G: ClassTag: FiniteGroup] = BasicAlgorithms.randomized(Random)

  def fromChain[G: ClassTag: FiniteGroup: Representations](chain: Chain[G],
    representationOption: RefOption[Representation[G]] = RefNone, givenGenerators: RefOption[Iterable[G]] = RefNone): Grp[G] = {
    val representation = representationOption.getOrElse(Representations[G].get(chain.generators))
    chain match {
      case node: Node[G] if representation.action != node.action =>
        new GrpLazy(givenGenerators.getOrElse(chain.generators), RefSome(chain.order), RefSome(chain.randomElement(_)), representationOption)
      case _ =>
        new GrpChain[G](givenGenerators.getOrElse(chain.generators), representation, chain)
    }
  }

  def fromGenerators[G: ClassTag: FiniteGroup: Representations](generators: Iterable[G],
    representationOption: RefOption[Representation[G]] = RefNone): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId), givenRepresentation = representationOption)

  def apply[G: ClassTag: FiniteGroup: Representations](generators: G*): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId))

  def fromGeneratorsAndOrder[G: ClassTag: FiniteGroup: Representations](generators: Iterable[G], order: BigInt,
    representationOption: RefOption[Representation[G]] = RefNone): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId), givenOrder = RefSome(order), givenRepresentation = representationOption)

  def fromSubgroup[S, G: ClassTag: FiniteGroup: Representations](subgroup: S,
    representationOption: RefOption[Representation[G]] = RefNone)(implicit sg: Subgroup[S, G]): Grp[G] =
    new GrpLazy[G](subgroup.generators,
      givenOrder = RefSome(subgroup.order),
      givenRandomElement = RefSome(subgroup.randomElement(_)),
      givenRepresentation = representationOption)

  implicit def GrpSubgroup[G](implicit algebra: FiniteGroup[G]): Subgroup[Grp[G], G] = new GrpSubgroup[G]
  implicit def GrpSubgroups[G](grp: Grp[G]): GrpSubgroups[G] = new GrpSubgroups[G](grp)
  implicit def GrpLexElements[G](grp: Grp[G]): GrpLexElements[G] = new GrpLexElements[G](grp)
}

class GrpSubgroup[G](implicit val algebra: FiniteGroup[G]) extends Subgroup[Grp[G], G] {
  def iterator(grp: Grp[G]) = {
    import grp.gClassTag
    grp.chain.iterator
  }
  def generators(grp: Grp[G]) = grp.generators
  def order(grp: Grp[G]) = grp.order
  def randomElement(grp: Grp[G], random: Random) = grp.randomElement(random)
  override def contains(grp: Grp[G], g: G) = grp.contains(g)
  override def toGrp(grp: Grp[G])(implicit classTag: ClassTag[G], representations: Representations[G]): Grp[G] = grp
}
