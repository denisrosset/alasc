package net.alasc
package math

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group, PartialOrder}
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}
import spire.syntax.group._
import spire.syntax.partialOrder._
import spire.util.Opt

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
    case Opt(o) => s" of order $o"
    case _ => ""
  })

  /** Set of algorithms used in the computations. */
  implicit def algorithms: BasicAlgorithms[G]

  /** ClassTag for elements. */
  implicit def classTag: ClassTag[G] = algorithms.classTag

  /** Representation provider for elements of G, used only if needed. */
  implicit def representations: Representations[G]

  /** Generators of the group. Must not contain the identity. */
  def generators: Iterable[G]

  /** Iterator through the group elements. */
  def iterator: Iterator[G]

  /** Finite group operations on type G. */
  implicit def group: Group[G] = algorithms.group
  implicit def equ: Eq[G] = algorithms.equ

  override def equals(any: Any) = any match {
    case that: Grp[G] => Grp.partialOrder[G].eqv(this, that)
    case _ => false
  }

  def isTrivial: Boolean = chainIfComputed match {
    case Opt(c) => c.isTrivial
    case _ => generators.isEmpty
  }

  def order: BigInt
  def orderIfComputed: Opt[BigInt]
  def isChainComputed = chainIfComputed.nonEmpty
  def chainIfComputed: Opt[Chain[G]]
  def chain: Chain[G]
  def chain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Chain[G]
  def withComputedChain(representation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Grp[G] =
    Grp.fromChain(chain(representation, baseGuide), Opt(representation))
  def representation: Representation[G]
  def representationIfComputed: Opt[Representation[G]]
  def randomElement(random: Random): G
  def contains(g: G): Boolean

  def conjBy(g: G, gInv: G): Grp[G] = chainIfComputed match {
    case Opt(chain) if representation.represents(g) =>
      GrpConjugated(algorithms, generators, representation, chain, g, gInv)
    case _ =>
      val conjRepresentation = representationIfComputed.filter(_.represents(g))
      orderIfComputed match {
        case Opt(ord) => Grp.fromGeneratorsAndOrder(generators.map(f => gInv |+| f |+| g), ord, conjRepresentation)
        case _ => Grp.fromGenerators(generators.map(f => gInv |+| f |+| g), conjRepresentation)
      }
  }

  def reducedGenerators: Grp[G] = {
    implicit def nodeBuilder: NodeBuilder[G] = algorithms.nodeBuilder
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
  def chainIfComputed = Opt(chain)
  def order = chain.order
  def orderIfComputed = Opt(order)
  def randomElement(random: Random) = chain.randomElement(random)
  def iterator: Iterator[G] = chain.elementsIterator
  def representationIfComputed = Opt(representation)
  def contains(g: G) = chain.sifts(g)
}

/** Represents a conjugated group from an original group G (represented by `originalChain`) conjugated by g (with gInv == g.inverse).
  * The represented group is `H = gInv G g`.
  * 
  * @note The `representation` must be able to represent `g`.
  */
case class GrpConjugated[G](algorithms: BasicAlgorithms[G], originalGenerators: Iterable[G], representation: Representation[G], originalChain: Chain[G], g: G, gInv: G)(implicit val representations: Representations[G]) extends Grp[G] { lhs =>
  originalChain match {
    case node: Node[G] => require(node.action == representation.action)
    case _: Term[G] =>
  }
  def representationIfComputed = Opt(representation)
  def chainIfComputed = Opt(chain)
  override def isTrivial: Boolean = originalChain.isTrivial
  def chain = originalChain match {
    case node: Node[G] =>
      implicit def action = representation.action
      import algorithms.nodeBuilder
      val mut = algorithms.mutableChain(node)(node.action)
      mut.conjugate(g, gInv)
      mut.toChain
    case term: Term[G] => term
  }
  def chain(givenRepresentation: Representation[G], baseGuide: BaseGuide = BaseGuide.empty) = originalChain match {
    case node: Node[G] =>
      if (node.action == givenRepresentation.action) {
        implicit def action = givenRepresentation.action
        import algorithms.nodeBuilder
        val mut = algorithms.mutableChain(node)
        mut.conjugate(g, gInv)
        algorithms.changeBaseSameAction(mut, baseGuide)
        mut.toChain
      } else
        algorithms.chainWithBase(generators, randomElement(_), order, baseGuide, givenRepresentation.action)
    case term: Term[G] => term
  }
  def order = originalChain.order
  def orderIfComputed = Opt(order)
  def randomElement(random: Random) = {
    val h = originalChain.randomElement(random)
    gInv |+| h |+| g
  }
  // `h in gInv G g` if and only if `g h gInv in G`.
  def contains(h: G) = originalChain.sifts(g |+| h |+| gInv)

  def iterator: Iterator[G] = originalChain.elementsIterator.map(h => gInv |+| h |+| g)

  def generators = originalChain.strongGeneratingSet.map(h => gInv |+| h |+| g)
  override def conjBy(h: G, hInv: G): Grp[G] =
    if (representation.represents(h))
      new GrpConjugated(algorithms, originalGenerators, representation, originalChain, g |+| h, hInv |+| gInv)
    else
      super.conjBy(h, hInv)
}

abstract class GrpLazyBase[G] extends Grp[G] {
  def isOrderComputed: Boolean

  protected def computeChain(givenRepresentation: Opt[Representation[G]] = Opt.empty[Representation[G]]): Chain[G]

  def iterator: Iterator[G] = chain.elementsIterator

  def chain: Chain[G] = chain(representation)

  def chain(representationToUse: Representation[G], baseGuide: BaseGuide = BaseGuide.empty): Chain[G] =
    chainIfComputed match {
      case Opt(node: Node[G]) => algorithms.chainWithBase(node, baseGuide, representationToUse.action)
      case Opt(term: Term[G]) => term
      case _ =>
        computeChain(Opt(representationToUse))
        chain(representationToUse, baseGuide)
    }
}


object Grp {

  implicit def lattice[G:Representations:BasicAlgorithms]: Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] = new GrpLattice[G] {
    def algorithms = implicitly[BasicAlgorithms[G]]
    def representations = implicitly[Representations[G]]
  }

  implicit def partialOrder[G]: PartialOrder[Grp[G]] = new GrpPartialOrder[G] { }

  implicit def defaultAlgorithms[G](implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]) = BasicAlgorithms.randomized(Random)(classTag, equ, group)

  def fromChain[G:ClassTag:Eq:Group:Representations](
    chain: Chain[G],
    representationOption: Opt[Representation[G]] = Opt.empty[Representation[G]],
    givenGenerators: Opt[Iterable[G]] = Opt.empty[Iterable[G]]
  ): Grp[G] = {
    val representation = representationOption.getOrElse(Representations[G].get(chain.strongGeneratingSet))
    chain match {
      case node: Node[G] if representation.action != node.action =>
        new GrpLazy(givenGenerators.getOrElse(chain.strongGeneratingSet), Opt(chain.order), Opt(chain.randomElement(_)), representationOption)
      case _ =>
        new GrpChain[G](givenGenerators.getOrElse(chain.strongGeneratingSet), representation, chain)
    }
  }

  def fromGenerators[G:ClassTag:Eq:Group:Representations](
    generators: Iterable[G],
    representationOption: Opt[Representation[G]] = Opt.empty[Representation[G]]
  ): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId), givenRepresentation = representationOption)

  def apply[G:ClassTag:Eq:Group:Representations](generators: G*): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId))

  def trivial[G:ClassTag:Eq:Group:Representations]: Grp[G] = new GrpChain[G](Iterable.empty, Representations[G].lattice.zero, new Term[G])

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group:Representations](generators: Iterable[G], order: BigInt,
    representationOption: Opt[Representation[G]] = Opt.empty[Representation[G]]): Grp[G] =
    new GrpLazy[G](generators.filterNot(_.isId), givenOrder = Opt(order), givenRepresentation = representationOption)

  implicit def subgroups[G](grp: Grp[G]): GrpSubgroups[G] = new GrpSubgroups[G](grp)
  implicit def lexElements[G](grp: Grp[G]): GrpLexElements[G] = new GrpLexElements[G](grp)

}

trait GrpPartialOrder[G] extends PartialOrder[Grp[G]] {

  override def eqv(x: Grp[G], y: Grp[G]): Boolean = (x.order == y.order) && lteqv(x, y)
  override def lteqv(x: Grp[G], y: Grp[G]): Boolean = x.generators.forall(y.contains)
  override def gteqv(x: Grp[G], y: Grp[G]): Boolean = y.generators.forall(x.contains)
  override def lt(x: Grp[G], y: Grp[G]): Boolean = (x.order < y.order) && lteqv(x, y)
  override def gt(x: Grp[G], y: Grp[G]): Boolean = (x.order > y.order) && gteqv(x, y)

  def partialCompare(x: Grp[G], y: Grp[G]): Double = {
    val c = x.order.compare(y.order)
    if (c < 0) {
      if (lteqv(x, y)) -1.0 else Double.NaN
    } else if (c > 0) {
      if (gteqv(x, y)) 1.0 else Double.NaN
    } else { // c == 0
      if (lteqv(x, y)) 0.0 else Double.NaN
    }
  }

}

trait GrpLattice[G] extends Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] {

  implicit def algorithms: BasicAlgorithms[G]
  implicit def classTag: ClassTag[G] = algorithms.classTag
  implicit def representations: Representations[G]

  implicit def group: Group[G] = algorithms.group
  implicit def equ: Eq[G] = algorithms.equ

  def zero = Grp.fromGenerators[G](Iterable.empty)

  def joinRepresentation(lhs: Grp[G], rhs: Grp[G]): Representation[G] = lhs.representationIfComputed match {
    case Opt(lhsRepr) => rhs.representationIfComputed match {
      case Opt(rhsRepr) => representations.repJoin(lhsRepr, rhsRepr, lhs.generators, rhs.generators)
      case _ => representations.repJoin(lhsRepr, lhs.generators, rhs.generators)
    }
    case _ => rhs.representationIfComputed match {
      case Opt(rhsRepr) => representations.repJoin(rhsRepr, rhs.generators, lhs.generators)
      case _ => representations.get(lhs.generators ++ rhs.generators)
    }
  }

  protected def unionByAdding(chain: Chain[G], rp: Representation[G], generators: Iterable[G]): Grp[G] = {
    val mutableChain = algorithms.mutableChainCopyWithAction(chain, rp.action)
    algorithms.insertGenerators(mutableChain, generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain, Opt(rp))
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
