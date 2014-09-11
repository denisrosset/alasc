package net.alasc
package math

import scala.annotation.tailrec
import scala.util.Random

import spire.syntax.group._
import spire.syntax.groupAction._

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

  def fixingPartitionW(partition: Domain#Partition, rp: Representation[G]): Grp[G]
  def fixingPartition(partition: Domain#Partition)(implicit prp: PermutationRepresentations[G]): Grp[G]
  def stabilizerW(b: Int, rp: Representation[G]): (Grp[G], Transversal[G])
  def stabilizer(b: Int)(implicit prp: PermutationRepresentations[G]): (Grp[G], Transversal[G])
  def pointwiseStabilizerW(set: Set[Int], rp: Representation[G]): Grp[G]
  def pointwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G]
  def setwiseStabilizer(set: Set[Int], rp: Representation[G]): Grp[G]
  def setwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G]

  // operations between subgroups
  def hasSubgroup(rhs: Grp[G]): Boolean = rhs.generators.forall(g => lhs.contains(g))
  def hasProperSubgroup(rhs: Grp[G]): Boolean = hasSubgroup(rhs) && (lhs.order != rhs.order)
  def isSubgroupOf(rhs: Grp[G]): Boolean = rhs.hasSubgroup(lhs)
  def isProperSubgroupOf(rhs: Grp[G]): Boolean = rhs.hasProperSubgroup(lhs)

  def &(rhs: Grp[G]) = intersect(rhs)
  def |(rhs: Grp[G]) = union(rhs)
  def union(rhs: Grp[G]): Grp[G]
  def intersect(rhs: Grp[G]): Grp[G]
  def /(rhs: Grp[G]): LeftCosets[G] = {
    require(rhs.generators.forall(lhs.contains(_)))
    new LeftCosets(lhs, rhs)
  }
  def \(rhs: Grp[G]): RightCosets[G] = {
    require(lhs.generators.forall(rhs.contains(_)))
    new RightCosets(lhs, rhs)
  }
  def lexElements(implicit rp: Representation[G]): coll.big.IndexedSet[G]
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

  def newMutableChain(rp: Representation[G], givenBase: Seq[Int] = Seq.empty): MutableChain[G] =
    algorithms.mutableChainCopyWithAction(chain(RefSome(rp), givenBase), rp.action)

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

  // operations on this Grp alone

  def fixingPartitionW(partition: Domain#Partition, rp: Representation[G]): Grp[G] =
    Grp.fromChain(algorithms.fixingPartition(chain, partition)(rp.action).toChain, RefSome(rp))

  def fixingPartition(partition: Domain#Partition)(implicit prp: PermutationRepresentations[G]): Grp[G] =
    fixingPartitionW(partition, prp.forSize(partition.size))

  def stabilizerW(b: Int, rp: Representation[G]): (Grp[G], Transversal[G]) =  {
    val newChain = algorithms.withBase(chain(RefSome(rp), Seq(b)), Seq(b))(rp.action)
    val (nextChain, transversal) = newChain.detach(b)
    (Grp.fromChain(nextChain, RefSome(rp)), transversal)
  }

  def stabilizer(b: Int)(implicit prp: PermutationRepresentations[G]): (Grp[G], Transversal[G]) = {
    val rp = if (b < representation.size) representation else prp.forSize(b + 1)
    stabilizerW(b, rp)
  }

  def pointwiseStabilizerW(set: Set[Int], rp: Representation[G]): Grp[G] = {
    val mutableChain = algorithms.pointwiseStabilizer(chain, set)(rp.action)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
  }

  def pointwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G] = {
    if (points.size == 0) return this
    val set = Set(points:_*)
    val maxSet = set.max
    val rp = if (maxSet < representation.size) representation else prp.forSize(maxSet + 1)
    pointwiseStabilizerW(set, rp)
  }

  def setwiseStabilizer(set: Set[Int], rp: Representation[G]): Grp[G] = {
    val mutableChain = algorithms.setwiseStabilizer(chain, set)(rp.action)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
  }

  def setwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G] = {
    if (points.size == 0) return this
    val set = Set(points:_*)
    val maxSet = set.max
    val rp = if (maxSet < representation.size) representation else prp.forSize(maxSet + 1)
    setwiseStabilizer(set, rp)
  }

  // operations between subgroups, with possible action reconfiguration

  def joinRepresentation(rhs: Grp[G]): Representation[G] = lhs.representationIfComputed match {
    case RefOption(lhsRepr) => rhs.representationIfComputed match {
      case RefOption(rhsRepr) => representations.repJoin(lhsRepr, rhsRepr, lhs.generators, rhs.generators)
      case _ => representations.repJoin(lhsRepr, lhs.generators, rhs.generators)
    }
    case _ => rhs.representationIfComputed match {
      case RefOption(rhsRepr) => representations.repJoin(rhsRepr, rhs.generators, lhs.generators)
      case _ => representations.get(lhs.generators ++ rhs.generators)
    }
  }

  protected def unionByAdding(chain: Chain[G], rp: Representation[G], generators: Iterable[G]): Grp[G] = {
    val mutableChain = newMutableChain(rp)
    algorithms.insertGenerators(mutableChain, generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
  }

  def union(rhs: Grp[G]): Grp[G] = {
    // if one of the arguments has a computed chain with a representation compatible with the other argument generators,
    // augment the computed chain with these generators
    if (lhs.chainIfComputed.nonEmpty && rhs.generators.forall(g => lhs.representation.represents(g)))
      return unionByAdding(lhs.chain, lhs.representation, rhs.generators)
    if (rhs.chainIfComputed.nonEmpty && lhs.generators.forall(g => rhs.representation.represents(g)))
      return unionByAdding(rhs.chain, rhs.representation, lhs.generators)
    // if representations are known but not compatible, use the join of the representations for the union
    val rp = joinRepresentation(rhs)
    if (lhs.orderIfComputed.nonEmpty) {
      if (rhs.orderIfComputed.nonEmpty) {
        if (lhs.order >= rhs.order)
          unionByAdding(lhs.chain(RefSome(rp)), rp, rhs.generators)
        else
          unionByAdding(rhs.chain(RefSome(rp)), rp, lhs.generators)
      } else
        unionByAdding(lhs.chain(RefSome(rp)), rp, rhs.generators)
    } else {
      if (rhs.orderIfComputed.nonEmpty)
        unionByAdding(rhs.chain(RefSome(rp)), rp, lhs.generators)
      else
        Grp.fromGenerators(lhs.generators ++ rhs.generators, RefSome(rp))
    }
  }

  def intersect(rhs: Grp[G]): Grp[G] = {
    def grpFromChains(lChain: Chain[G], rChain: Chain[G], rp: Representation[G]): Grp[G] =
      Grp.fromChain(algorithms.intersection(lChain, rChain)(rp.action).toChain, RefSome(rp))
    if (lhs.chainIfComputed.nonEmpty && rhs.chainIfComputed.nonEmpty) {
      val lCompatible = rhs.generators.forall(g => lhs.representation.represents(g))
      val rCompatible = lhs.generators.forall(g => rhs.representation.represents(g))
      if (lCompatible && (!rCompatible || lhs.order >= rhs.order))
        grpFromChains(lhs.chain, rhs.chain(RefSome(lhs.representation), lhs.chain.base), lhs.representation)
      else
        grpFromChains(rhs.chain, lhs.chain(RefSome(rhs.representation), rhs.chain.base), rhs.representation)
    } else {
      val rp = joinRepresentation(rhs)
      val lChain = lhs.chain(RefSome(rp))
      val rChain = rhs.chain(RefSome(rp), lChain.base) // TODO: use BaseGuideSeqStripped
      grpFromChains(lChain, rChain, rp)
    }
  }

  // enumeration of subgroup elements
  def lexElements(implicit rp: Representation[G]): coll.big.IndexedSet[G] = new coll.big.IndexedSet[G] {
    implicit val action = rp.action
    val lexChain = algorithms.withBase(chain, BaseGuideLex(rp.size))(rp.action)
    def size = coll.BigIntSize(lhs.order)
    def length = lhs.order
    def contains(g: G) = (lhs: Grp[G]).contains(g)
    def foreach[U](f: G => U) = iterator.foreach(f)
    def apply(idx: BigInt): G = {
      @tailrec def rec(current: Chain[G], curIdx: BigInt, curOrder: BigInt, curG: G): G = current match {
        case node: Node[G] =>
          val sortedOrbit = node.orbit.toSeq.sortBy(k => k <|+| curG)
          val nextOrder = curOrder / node.orbitSize
          val nextIdx = curIdx % nextOrder
          val orbitIndex = ((curIdx - nextIdx) / nextOrder).toInt
          val nextG = node.u(sortedOrbit(orbitIndex)) |+| curG
          rec(node.next, nextIdx, nextOrder, nextG)
        case _: Term[G] =>
          assert(curIdx == 0)
          curG
      }
      rec(lexChain, idx, lhs.order, algebra.id)
    }
    def iterator: Iterator[G] = {
      def rec(current: Chain[G], curG: G): Iterator[G] = current match {
        case node: Node[G] =>
          val sortedOrbit = node.orbit.toSeq.sortBy(k => k <|+| curG)
          for {
            b <- sortedOrbit.iterator
            nextG = node.u(b) |+| curG
            rest <- rec(node.next, nextG)
          } yield rest
        case _: Term[G] => Iterator(curG)
      }
      rec(lexChain, algebra.id)
    }
  }
}

object Grp {
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

class GrpLattice[G](implicit val algebra: FiniteGroup[G], representations: Representations[G], algorithms: BasicAlgorithms[G]) extends BoundedBelowLattice[Grp[G]] {
  def zero = Grp.fromGenerators[G](Iterable.empty, RefNone)

  def joinRepresentation(lhs: Grp[G], rhs: Grp[G]): Representation[G] = lhs.representationIfComputed match {
    case RefOption(lhsRepr) => rhs.representationIfComputed match {
      case RefOption(rhsRepr) => representations.repJoin(lhsRepr, rhsRepr, lhs.generators, rhs.generators)
      case _ => representations.repJoin(lhsRepr, lhs.generators, rhs.generators)
    }
    case _ => rhs.representationIfComputed match {
      case RefOption(rhsRepr) => representations.repJoin(rhsRepr, rhs.generators, lhs.generators)
      case _ => representations.get(lhs.generators ++ rhs.generators)
    }
  }

  protected def unionByAdding(chain: Chain[G], rp: Representation[G], generators: Iterable[G]): Grp[G] = {
    val mutableChain = algorithms.mutableChainCopyWithAction(chain, rp.action)
    algorithms.insertGenerators(mutableChain, generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
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
          unionByAdding(lhs.chain(RefSome(rp)), rp, rhs.generators)
        else
          unionByAdding(rhs.chain(RefSome(rp)), rp, lhs.generators)
      } else
        unionByAdding(lhs.chain(RefSome(rp)), rp, rhs.generators)
    } else {
      if (rhs.orderIfComputed.nonEmpty)
        unionByAdding(rhs.chain(RefSome(rp)), rp, lhs.generators)
      else
        Grp.fromGenerators(lhs.generators ++ rhs.generators, RefSome(rp))
    }
  }

  def meet(lhs: Grp[G], rhs: Grp[G]): Grp[G] = {
    def grpFromChains(lChain: Chain[G], rChain: Chain[G], rp: Representation[G]): Grp[G] =
      Grp.fromChain(algorithms.intersection(lChain, rChain)(rp.action).toChain, RefSome(rp))
    if (lhs.chainIfComputed.nonEmpty && rhs.chainIfComputed.nonEmpty) {
      val lCompatible = rhs.generators.forall(g => lhs.representation.represents(g))
      val rCompatible = lhs.generators.forall(g => rhs.representation.represents(g))
      if (lCompatible && (!rCompatible || lhs.order >= rhs.order))
        grpFromChains(lhs.chain, rhs.chain(RefSome(lhs.representation), lhs.chain.base), lhs.representation)
      else
        grpFromChains(rhs.chain, lhs.chain(RefSome(rhs.representation), rhs.chain.base), rhs.representation)
    } else {
      val rp = joinRepresentation(lhs, rhs)
      val lChain = lhs.chain(RefSome(rp))
      val rChain = rhs.chain(RefSome(rp), lChain.base) // TODO: use BaseGuideSeqStripped
      grpFromChains(lChain, rChain, rp)
    }
  }

  override def lteqv(x: Grp[G], y: Grp[G]): Boolean = x.generators.forall(y.contains(_))
  override def gteqv(x: Grp[G], y: Grp[G]): Boolean = y.generators.forall(x.contains(_))
  override def eqv(x: Grp[G], y: Grp[G]): Boolean = (x.order == y.order) && lteqv(x, y)
  override def lt(x: Grp[G], y: Grp[G]): Boolean = lteqv(x, y) && (x.order < y.order)
  override def gt(x: Grp[G], y: Grp[G]): Boolean = gteqv(x, y) && (x.order > y.order)

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
