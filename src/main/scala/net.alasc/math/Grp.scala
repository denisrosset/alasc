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

/** User-friendly representation of a group internally using a base and strong generating set data structure.
  *
  * Can be constructed from any finite group with a faithful permutation action.
  */
class Grp[G](
  val algorithms: BasicAlgorithms[G],
  val generators: Iterable[G],
  givenOrder: RefOption[BigInt] = RefNone,
  givenChain: RefOption[Chain[G]] = RefNone,
  givenRandomElement: RefOption[Function1[Random, G]] = RefNone)(
  implicit val algebra: FiniteGroup[G], actions: FaithfulPermutationActions[G]) { lhs =>

  // TODO conjugatedBy

  require(givenChain.fold(true)(_.isImmutable))

  private[this] var knownChain: RefOption[Chain[G]] = givenChain

  def actionFromGenerators = actions.actionFor(generators)
  def defaultAction = knownChain.fold(actionFromGenerators)(chain => chain.mapOrElse(node => node.action, actionFromGenerators))

  def chain: Chain[G] = chain()
  def chain(givenAction: FaithfulPermutationAction[G] = defaultAction, givenBase: Seq[Int] = Seq.empty): Chain[G] = {
    def computeChain(implicit action: FaithfulPermutationAction[G]): Chain[G] = knownOrder match {
      case RefOption(order) => algorithms match {
        case ssr: SchreierSimsRandomized[G] => ssr.randomizedSchreierSims(randomElement(_), order, givenBase).toChain
        case _ => algorithms.completeChainFromGeneratorsAndOrder(generators, order, givenBase).toChain
      }
      case _ => algorithms.completeChainFromGenerators(generators, givenBase).toChain
    }
    knownChain match {
      case RefOption(node: Node[G]) if node.action == givenAction => return node
      case RefOption(node: Node[G]) => computeChain(givenAction)
      case RefOption(term: Term[G]) => return term
      case _ =>
        val res = computeChain(givenAction)
        knownChain = RefSome(res)
        res
    }
  }

  private[this] var knownOrder: RefOption[BigInt] = givenOrder
  def order: BigInt = knownOrder.getOrElse {
    val o = chain.order
    knownOrder = RefSome(o)
    o
  }

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

  override def toString = generators.mkString("Grp(", ", ", ")") + (if (knownOrder.nonEmpty || knownChain.nonEmpty) s" of order ${order}" else "")

  // operations on this Grp alone

  def fixingSequence(seq: Seq[Any])(implicit action: FaithfulPermutationAction[G]): Grp[G] =
    Grp.fromChain(algorithms.fixingSequence(chain, seq)(action).toChain)(algebra, actions)

  def stabilizer(b: Int)(implicit action: FaithfulPermutationAction[G]): (Grp[G], Transversal[G]) = {
    val mutableChain = algorithms.mutableCopyWithAction(chain, action)
    algorithms.changeBase(mutableChain, Seq(b))
    val transversal: Transversal[G] = mutableChain.detachFirstNode(b)(algorithms.nodeBuilder, algebra, action)
    (Grp.fromChain(mutableChain.toChain), transversal)
  }

  def pointwiseStabilizer(set: Int*)(implicit action: FaithfulPermutationAction[G]): Grp[G] =
      pointwiseStabilizer(collection.immutable.BitSet.empty ++ set)
  def pointwiseStabilizer(set: Set[Int])(implicit action: FaithfulPermutationAction[G]): Grp[G] = {
    val mutableChain = algorithms.pointwiseStabilizer(chain, set)(action)
    Grp.fromChain(mutableChain.toChain)
  }

  def setwiseStabilizer(set: Int*)(implicit action: FaithfulPermutationAction[G]): Grp[G] =
    setwiseStabilizer(collection.immutable.BitSet.empty ++ set)
  def setwiseStabilizer(set: Set[Int])(implicit action: FaithfulPermutationAction[G]): Grp[G] = {
    val mutableChain = algorithms.setwiseStabilizer(chain, set)(action)
    Grp.fromChain(mutableChain.toChain)
  }

  // operations between subgroups
  def hasProperSubgroup(rhs: Grp[G]): Boolean = hasSubgroup(rhs) && (lhs.order != rhs.order)
  def hasSubgroup(rhs: Grp[G]): Boolean = rhs.generators.forall(g => lhs.contains(g))
  def isSubgroupOf(rhs: Grp[G]): Boolean = rhs.hasSubgroup(lhs)
  def isProperSubgroupOf(rhs: Grp[G]): Boolean = rhs.hasProperSubgroup(lhs)

  // operations between subgroups, with possible action reconfiguration

  def &(rhs: Grp[G]) = intersect(rhs)
  def |(rhs: Grp[G]) = union(rhs)

  def union(rhs: Grp[G]): Grp[G] = {
    if (lhs.order == 1) return rhs
    if (rhs.order == 1) return lhs
    lhs.chain match {
      case node: Node[G] if rhs.generators.forall(node.action.compatibleWith(_)) =>
        val mutableChain = algorithms.mutableChainCopy(node)(node.action)
        algorithms.insertGenerators(mutableChain, rhs.generators)
        algorithms.completeStrongGenerators(mutableChain)
        Grp.fromChain(mutableChain.toChain)
      case _ => rhs.chain match {
        case node: Node[G] if lhs.generators.forall(node.action.compatibleWith(_)) =>
          val mutableChain = algorithms.mutableChainCopy(node)(node.action)
          algorithms.insertGenerators(mutableChain, lhs.generators)
          algorithms.completeStrongGenerators(mutableChain)
          Grp.fromChain(mutableChain.toChain)
        case _ =>
          Grp.fromGenerators(lhs.generators ++ rhs.generators)
      }
    }
  }

  def intersect(rhs: Grp[G]): Grp[G] = {
    val lNode = lhs.chain match {
      case _: Term[G] => return lhs
      case node: Node[G] => node
    }
    val rNode = lhs.chain match {
      case _: Term[G] => return rhs
      case node: Node[G] => node
    }
    if (lNode.action == rNode.action) {
      implicit def action = lNode.action
      Grp.fromChain(algorithms.intersection(lNode, rNode).toChain)
    } else {
      if (rhs.generators.forall(lNode.action.compatibleWith(_))) {
        implicit def action = lNode.action
        val newR = algorithms.withAction(rNode, action)
        Grp.fromChain(algorithms.intersection(lNode, newR).toChain)
      } else if (lhs.generators.forall(rNode.action.compatibleWith(_))) {
        implicit def action = rNode.action
        val newL = algorithms.withAction(lNode, action)
        Grp.fromChain(algorithms.intersection(newL, rNode).toChain)
      } else {
        implicit val action = actions.actionFor(lhs.generators ++ rhs.generators)
        val newL = algorithms.withAction(lNode, action)
        val newR = algorithms.withAction(rNode, action)
        Grp.fromChain(algorithms.intersection(newL, newR).toChain)
      }
    }
  }

  def /(rhs: Grp[G]): LeftCosets[G] = {
    require(rhs.generators.forall(lhs.contains(_)))
    new LeftCosets(lhs, rhs)
  }
  def \(rhs: Grp[G]): RightCosets[G] = {
    require(lhs.generators.forall(rhs.contains(_)))
    new RightCosets(lhs, rhs)
  }

  // enumeration of subgroup elements
  def lexElements(implicit action: FaithfulPermutationAction[G]): coll.big.IndexedSet[G] = new coll.big.IndexedSet[G] {
    val lexChain = {
      val mutableChain = algorithms.mutableCopyWithAction(chain, action)
      algorithms.changeBase(mutableChain, BaseGuideLex(mutableChain.start.next.supportMax.getOrElse(-1) + 1))
      mutableChain.toChain
    }
    def size = coll.BigIntSize(lhs.order)
    def length = lhs.order
    def contains(g: G) = lhs.contains(g)
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
  def defaultAlg[G](implicit algebra: FiniteGroup[G]) = BasicAlgorithms.randomized(Random)
  def fromChain[G](chain: Chain[G])(implicit algebra: FiniteGroup[G], actions: FaithfulPermutationActions[G]) =
    new Grp[G](algorithms = defaultAlg[G], generators = chain.generators, givenChain = RefSome(chain))
  def fromGenerators[G](generators: Iterable[G])(implicit algebra: FiniteGroup[G], actions: FaithfulPermutationActions[G]) =
    new Grp[G](algorithms = defaultAlg[G], generators = generators)
  def apply[G](generators: G*)(implicit algebra: FiniteGroup[G], actions: FaithfulPermutationActions[G]) =
    new Grp[G](algorithms = defaultAlg[G], generators = generators)
  def fromGeneratorsAndOrder[G](generators: Iterable[G], order: BigInt)(implicit algebra: FiniteGroup[G], actions: FaithfulPermutationActions[G]) =
    new Grp[G](algorithms = defaultAlg[G], generators = generators, givenOrder = RefSome(order))
  def fromSubgroup[S, G](subgroup: S)(implicit algebra: FiniteGroup[G], sg: Subgroup[S, G], actions: FaithfulPermutationActions[G]) = {
    val alg = defaultAlg[G]
    new Grp[G](algorithms = defaultAlg[G], generators = subgroup.generators, givenOrder = RefSome(subgroup.order),
      givenRandomElement = RefSome(subgroup.randomElement(_)))
  }
  implicit def GrpSubgroup[G](implicit algebra: FiniteGroup[G]): Subgroup[Grp[G], G] = new GrpSubgroup[G]
}

class GrpSubgroup[G](implicit val algebra: FiniteGroup[G]) extends Subgroup[Grp[G], G] {
  def iterator(grp: Grp[G]) = grp.chain.iterator
  def generators(grp: Grp[G]) = grp.generators
  def order(grp: Grp[G]) = grp.order
  def randomElement(grp: Grp[G], random: Random) = grp.randomElement(random)
  override def contains(grp: Grp[G], g: G) = grp.chain.contains(g)
  override def toGrp(grp: Grp[G])(implicit actions: FaithfulPermutationActions[G]): Grp[G] = grp
}
