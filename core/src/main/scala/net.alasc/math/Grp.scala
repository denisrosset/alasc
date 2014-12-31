package net.alasc
package math

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.PartialOrder
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}
import spire.syntax.group._
import spire.syntax.partialOrder._
import spire.util.Nullbox

import net.alasc.algebra._
import net.alasc.math.bsgs._
import net.alasc.math.bsgs.algorithms._
import net.alasc.math.guide.{BaseGuide, BaseGuideSeq}
import net.alasc.syntax.all._

/** User-friendly representation of a group internally using a base and strong generating set data structure.
  *
  * Can be constructed from any finite group with a faithful permutation action.
  * 
  * The generators must never contain the identity.
  */
sealed abstract class Grp[G] { lhs =>
  override def hashCode = sys.error("HashCode not defined for Grp")
  override def toString = generators.mkString("Grp(", ", ", ")") + (orderIfComputed match {
    case Nullbox(o) => s" of order $o"
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
    case that: Grp[G] => Grp.subgroup[G].eqv(this, that)
    case _ => false
  }

  def isTrivial: Boolean = chainIfComputed match {
    case Nullbox(c) => c.isTrivial
    case _ => generators.isEmpty
  }

  def order: BigInt
  def orderIfComputed: Nullbox[BigInt]
  def isChainComputed = chainIfComputed.nonEmpty
  def chainIfComputed: Nullbox[Chain[G]]
  def chain: Chain[G]
  def chain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Chain[G]
  def withComputedChain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Grp[G] =
    Grp.fromChain(chain(representation, baseGuide), Nullbox(representation))
  def representation: Representation[G]
  def representationIfComputed: Nullbox[Representation[G]]
  def randomElement(random: Random): G
  def contains(g: G): Boolean

  def conjBy(ip: InversePair[G]): Grp[G] = chainIfComputed match {
    case Nullbox(chain) if representation.represents(ip.g) =>
      GrpConjugated(algorithms, generators, representation, chain, ip)
    case _ =>
      val conjRepresentation = representationIfComputed.filter(_.represents(ip.g))
      orderIfComputed match {
        case Nullbox(ord) => Grp.fromGeneratorsAndOrder(generators.map(_.conjBy(ip)), ord, conjRepresentation)
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
}

class GrpChain[G](val generators: Iterable[G], val representation: Representation[G], val chain: Chain[G])(implicit val algorithms: BasicAlgorithms[G], val representations: Representations[G]) extends Grp[G] { lhs =>
  chain match {
    case node: Node[G] => require(node.action == representation.action)
    case _: Term[G] =>
  }
  def chain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty) =
    algorithms.chainWithBase(chain, baseGuide, representation.action)
  def chainIfComputed = Nullbox(chain)
  def order = chain.order
  def orderIfComputed = Nullbox(order)
  def randomElement(random: Random) = chain.randomElement(random)
  def representationIfComputed = Nullbox(representation)
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
  def representationIfComputed = Nullbox(representation)
  def chainIfComputed = Nullbox(chain)
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
  def orderIfComputed = Nullbox(order)
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
  def isOrderComputed: Boolean

  protected def computeChain(givenRepresentation: Nullbox[Representation[G]] = Nullbox.empty[Representation[G]]): Chain[G]

  def chain: Chain[G] = chain(representation)

  def chain(representationToUse: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Chain[G] =
    chainIfComputed match {
      case Nullbox(node: Node[G]) => algorithms.chainWithBase(node, baseGuide, representationToUse.action)
      case Nullbox(term: Term[G]) => term
      case _ =>
        computeChain(Nullbox(representationToUse))
        chain(representationToUse, baseGuide)
    }
}


object Grp {
  implicit def lattice[G: ClassTag: FiniteGroup: Representations: BasicAlgorithms]: Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] = new GrpLattice[G] {
    def algorithms = implicitly[BasicAlgorithms[G]]
    def representations = implicitly[Representations[G]]
  }
  implicit def defaultAlgorithms[G: ClassTag: FiniteGroup] = BasicAlgorithms.randomized(Random)

  def fromChain[G: ClassTag: FiniteGroup: Representations](
    chain: Chain[G],
    representationOption: Nullbox[Representation[G]] = Nullbox.empty[Representation[G]],
    givenGenerators: Nullbox[Iterable[G]] = Nullbox.empty[Iterable[G]]
  ): Grp[G] = {
    val representation = representationOption.getOrElse(Representations[G].get(chain.generators))
    chain match {
      case node: Node[G] if representation.action != node.action =>
        new GrpLazy(givenGenerators.getOrElse(chain.generators), Nullbox(chain.order), Nullbox(chain.randomElement(_)), representationOption)
      case _ =>
        new GrpChain[G](givenGenerators.getOrElse(chain.generators), representation, chain)
    }
  }

  def fromGenerators[G: ClassTag: FiniteGroup: Representations](
    generators: Iterable[G],
    representationOption: Nullbox[Representation[G]] = Nullbox.empty[Representation[G]]
  ): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId), givenRepresentation = representationOption)

  def apply[G: ClassTag: FiniteGroup: Representations](generators: G*): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId))

  def trivial[G: ClassTag: FiniteGroup: Representations]: Grp[G] = new GrpChain[G](Iterable.empty, Representations[G].lattice.zero, new Term[G])

  def fromGeneratorsAndOrder[G: ClassTag: FiniteGroup: Representations](generators: Iterable[G], order: BigInt,
    representationOption: Nullbox[Representation[G]] = Nullbox.empty[Representation[G]]): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId), givenOrder = Nullbox(order), givenRepresentation = representationOption)

  def fromSubgroup[S, G: ClassTag: FiniteGroup: Representations](subgroup: S,
    representationOption: Nullbox[Representation[G]] = Nullbox.empty[Representation[G]])(implicit sg: Subgroup[S, G]): Grp[G] =
    new GrpLazy[G](subgroup.generators,
      givenOrder = Nullbox(subgroup.order),
      givenRandomElement = Nullbox(subgroup.randomElement(_)),
      givenRepresentation = representationOption)

  implicit def subgroup[G](implicit algebra: FiniteGroup[G]): Subgroup[Grp[G], G] = new GrpSubgroup[G]
  implicit def subgroups[G](grp: Grp[G]): GrpSubgroups[G] = new GrpSubgroups[G](grp)
  implicit def lexElements[G](grp: Grp[G]): GrpLexElements[G] = new GrpLexElements[G](grp)
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

trait GrpLattice[G] extends Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] {
  implicit def algorithms: BasicAlgorithms[G]
  implicit def representations: Representations[G]

  implicit def algebra: FiniteGroup[G] = algorithms.algebra
  implicit def gClassTag: ClassTag[G] = algorithms.gClassTag
  def zero = Grp.fromGenerators[G](Iterable.empty)

  def joinRepresentation(lhs: Grp[G], rhs: Grp[G]): Representation[G] = lhs.representationIfComputed match {
    case Nullbox(lhsRepr) => rhs.representationIfComputed match {
      case Nullbox(rhsRepr) => representations.repJoin(lhsRepr, rhsRepr, lhs.generators, rhs.generators)
      case _ => representations.repJoin(lhsRepr, lhs.generators, rhs.generators)
    }
    case _ => rhs.representationIfComputed match {
      case Nullbox(rhsRepr) => representations.repJoin(rhsRepr, rhs.generators, lhs.generators)
      case _ => representations.get(lhs.generators ++ rhs.generators)
    }
  }

  protected def unionByAdding(chain: Chain[G], rp: Representation[G], generators: Iterable[G]): Grp[G] = {
    val mutableChain = algorithms.mutableChainCopyWithAction(chain, rp.action)
    algorithms.insertGenerators(mutableChain, generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain, Nullbox(rp))
  }

  def join(lhs: Grp[G], rhs: Grp[G]): Grp[G] = {
    // if one of the arguments has a computed chain with a representation compatible with the other argument generators,
    // augment the computed chain with these generators
    if (lhs.chainIfComputed.nonEmpty && rhs.generators.forall(g => lhs.representation.represents(g)))
      return unionByAdding(lhs.chain, lhs.representation, rhs.generators)
    if (rhs.chainIfComputed.nonEmpty && lhs.generators.forall(g => rhs.representation.represents(g)))
      return unionByAdding(rhs.chain, rhs.representation, lhs.generators)
    // if representations are known but not compatible, use the join of the representations for the union
    val rp = joinRepresentation(lhs, rhs)
    if (lhs.orderIfComputed.nonEmpty) {
      if (rhs.orderIfComputed.nonEmpty) {
        if (lhs.order >= rhs.order)
          unionByAdding(lhs.chain(rp), rp, rhs.generators)
        else
          unionByAdding(rhs.chain(rp), rp, lhs.generators)
      } else
        unionByAdding(lhs.chain(rp), rp, rhs.generators)
    } else {
      if (rhs.orderIfComputed.nonEmpty)
        unionByAdding(rhs.chain(rp), rp, lhs.generators)
      else
        Grp.fromGenerators(lhs.generators ++ rhs.generators)
    }
  }

  def meet(lhs: Grp[G], rhs: Grp[G]): Grp[G] = {
    def grpFromChains(lChain: Chain[G], rChain: Chain[G], rp: Representation[G]): Grp[G] =
      Grp.fromChain(Intersection.intersection(lChain, rChain))
    if (lhs.chainIfComputed.nonEmpty && rhs.chainIfComputed.nonEmpty) {
      val lCompatible = rhs.generators.forall(g => lhs.representation.represents(g))
      val rCompatible = lhs.generators.forall(g => rhs.representation.represents(g))
      if (lCompatible && (!rCompatible || lhs.order >= rhs.order))
        grpFromChains(lhs.chain, rhs.chain(lhs.representation, BaseGuideSeq(lhs.chain.base)), lhs.representation)
      else
        grpFromChains(rhs.chain, lhs.chain(rhs.representation, BaseGuideSeq(rhs.chain.base)), rhs.representation)
    } else {
      val rp = joinRepresentation(lhs, rhs)
      val lChain = lhs.chain(rp)
      val rChain = rhs.chain(rp, BaseGuideSeq(lChain.base)) // TODO: use BaseGuideSeqStripped
      grpFromChains(lChain, rChain, rp)
    }
  }
}
