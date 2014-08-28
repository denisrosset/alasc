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
  val defaultAction: PermutationAction[G],
  givenGenerators: RefOption[Iterable[G]] = RefNone,
  givenOrder: RefOption[BigInt] = RefNone,
  givenChain: RefOption[Chain[G]] = RefNone,
  givenRandomElement: Option[Random => G] = None)(implicit val algebra: FiniteGroup[G]) { lhs =>
  require(givenChain.fold(true)(_.isImmutable))
  private[this] var knownChain: RefOption[Chain[G]] = givenChain
  def chain: Chain[G] = knownChain.getOrElse {
    implicit def action = defaultAction
    if (knownGenerators.nonEmpty && knownOrder.nonEmpty)
      algorithms.completeChainFromGeneratorsAndOrder(knownGenerators.get, knownOrder.get).toChain
    else if (knownRandomElement.nonEmpty && knownOrder.nonEmpty && algorithms.isInstanceOf[SchreierSimsRandomized[G]])
      algorithms.asInstanceOf[SchreierSimsRandomized[G]].randomizedSchreierSims(knownRandomElement.get, knownOrder.get).toChain
    else if (knownGenerators.nonEmpty)
      algorithms.completeChainFromGenerators(knownGenerators.get).toChain
    else
      sys.error("Group cannot be represented from the given information.")
  }
  private[this] var knownOrder: RefOption[BigInt] = givenOrder
  def order: BigInt = knownOrder.getOrElse {
    val o = chain.order
    knownOrder = RefSome(o)
    o
  }
  private[this] var knownGenerators: RefOption[Iterable[G]] = givenGenerators
  def generators: Iterable[G] = knownGenerators.getOrElse {
    val g = chain.generators
    knownGenerators = RefSome(g)
    g
  }
  private[this] val knownRandomElement: Option[Random => G] = givenRandomElement

  def chain(givenAction: PermutationAction[G], givenBase: Seq[Int] = Seq.empty): Chain[G] = {
    if (givenAction == defaultAction) {
      if (givenBase.isEmpty)
        chain
      else {
        val mutableChain = algorithms.mutableChainCopy(chain)(defaultAction)
        algorithms.changeBase(mutableChain, givenBase)(defaultAction)
        mutableChain.toChain
      }
    } else
      algorithms.completeChainFromSubgroup(chain, givenBase)(givenAction, implicitly[Subgroup[Chain[G], G]]).toChain
  }

  // TODO conjugatedBy
  override def toString = generators.mkString("Grp(", ", ", ")") + (if (knownOrder.nonEmpty || knownChain.nonEmpty) s" of order ${order}" else "")
  def randomElement(random: Random): G = knownRandomElement.fold(chain.randomElement(random))(_(random))
  def &(rhs: Grp[G]) = intersect(rhs)
  def |(rhs: Grp[G]) = union(rhs)
  def fixingSequence(seq: Seq[Any])(implicit action: PermutationAction[G]): Grp[G] =
    Grp.fromChain(algorithms.fixingSequence(chain, seq)(action).toChain)
  def intersect(rhs: Grp[G]): Grp[G] = Grp.fromChain(algorithms.intersection(chain, rhs.chain)(defaultAction).toChain)(algebra, defaultAction)
  def union(rhs: Grp[G]): Grp[G] = if (knownChain.nonEmpty || givenGenerators.isEmpty) { // TODO: something more clever if the two groups commute
    // TODO: when rhs has been computed but not lhs
    val mutableChain = algorithms.mutableCopyWithAction(chain, defaultAction)
    algorithms.insertGenerators(mutableChain, rhs.generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain)(algebra, defaultAction)
  } else {
    val mutableChain = algorithms.incompleteChainWithGenerators(lhs.generators ++ rhs.generators)(defaultAction)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain)(algebra, defaultAction)
  }
  def stabilizer(b: Int)(implicit action: PermutationAction[G]): (Grp[G], Transversal[G]) = {
    val mutableChain = algorithms.mutableCopyWithAction(chain, action)
    algorithms.changeBase(mutableChain, Seq(b))
    val transversal: Transversal[G] = mutableChain.detachFirstNode(b)(algorithms.nodeBuilder, algebra, action)
    (Grp.fromChain(mutableChain.toChain), transversal)
  }
  def pointwiseStabilizer(set: Int*)(implicit action: PermutationAction[G]): Grp[G] =
      pointwiseStabilizer(collection.immutable.BitSet.empty ++ set)
  def pointwiseStabilizer(set: Set[Int])(implicit action: PermutationAction[G]): Grp[G] = {
    val mutableChain = algorithms.pointwiseStabilizer(chain, set)(action)
    Grp.fromChain(mutableChain.toChain)
  }
  def setwiseStabilizer(set: Int*)(implicit action: PermutationAction[G]): Grp[G] =
    setwiseStabilizer(collection.immutable.BitSet.empty ++ set)
  def setwiseStabilizer(set: Set[Int])(implicit action: PermutationAction[G]): Grp[G] = {
    val mutableChain = algorithms.setwiseStabilizer(chain, set)(action)
    Grp.fromChain(mutableChain.toChain)
  }
  def /(rhs: Grp[G]): LeftCosets[G] = {
    require(rhs.generators.forall(lhs.contains(_)))
    new LeftCosets(lhs, rhs)
  }
  def \(rhs: Grp[G]): RightCosets[G] = {
    require(lhs.generators.forall(rhs.contains(_)))
    new RightCosets(lhs, rhs)
  }
  def lexElements(implicit action: PermutationAction[G]): coll.big.IndexedSet[G] = new coll.big.IndexedSet[G] {
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
  def fromChain[G](chain: Chain[G])(implicit algebra: FiniteGroup[G], action: PermutationAction[G]) =
    new Grp[G](defaultAction = action, algorithms = defaultAlg[G], givenChain = RefSome(chain))
  def apply[G](generators: G*)(implicit algebra: FiniteGroup[G], action: PermutationAction[G]) = {
    new Grp[G](defaultAction = action, algorithms = defaultAlg[G], givenGenerators = RefSome(generators))
  }
  def fromGeneratorsAndOrder[G](generators: Iterable[G], order: BigInt)(implicit algebra: FiniteGroup[G], action: PermutationAction[G]) =
    new Grp[G](defaultAction = action, algorithms = defaultAlg[G], givenGenerators = RefSome(generators), givenOrder = RefSome(order))
  def fromSubgroup[S, G](subgroup: S)(implicit algebra: FiniteGroup[G], sg: Subgroup[S, G], action: PermutationAction[G]) = {
    val alg = defaultAlg[G]
    new Grp[G](defaultAction = action, algorithms = defaultAlg[G],
      givenGenerators = RefSome(subgroup.generators), givenOrder = RefSome(subgroup.order),
      givenRandomElement = Some(subgroup.randomElement(_)))
  }
  implicit def GrpSubgroup[G](implicit algebra: FiniteGroup[G]): Subgroup[Grp[G], G] = new GrpSubgroup[G]
}

class GrpSubgroup[G](implicit val algebra: FiniteGroup[G]) extends Subgroup[Grp[G], G] {
  def iterator(grp: Grp[G]) = grp.chain.iterator
  def generators(grp: Grp[G]) = grp.generators
  def order(grp: Grp[G]) = grp.order
  def randomElement(grp: Grp[G], random: Random) = grp.randomElement(random)
  override def contains(grp: Grp[G], g: G) = grp.chain.contains(g)
  override def toGrp(grp: Grp[G])(implicit action: PermutationAction[G]): Grp[G] = grp
}
