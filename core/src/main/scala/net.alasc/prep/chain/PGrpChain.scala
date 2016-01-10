package net.alasc.prep
package chain

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group, Order}
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, Permutation}
import net.alasc.domains.Partition
import net.alasc.finite._

import bsgs._

abstract class PGrpChain[G] extends PGrp[G] { lhs =>

  implicit def builder: PGrpChainBuilder[G]
  implicit def baseChange: BaseChange = builder.baseChange
  implicit def baseSwap: BaseSwap = builder.baseSwap
  implicit def classTag: ClassTag[G] = builder.classTag
  implicit def schreierSims: SchreierSims = builder.schreierSims

  // enumeration of subgroup elements
  def lexElements: BigIndexedSeq[G] = new BigIndexedSeq[G] {
    implicit val action = pRep.permutationAction
    val lexChain = builder.fromChainIn(pRep, Opt(BaseGuideLex(pRep.size)))(chain).chain
    println(lexChain)
    def length = lhs.order
    def contains(g: G) = (lhs: Grp[G]).contains(g)
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
      rec(lexChain, idx, lhs.order, Group[G].id)
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
      rec(lexChain, Group[G].id)
    }
  }

  def subgroupFor(definition: SubgroupDefinition[G]): Grp[G]

  def chain: Chain[G]

  def chainOpt: Opt[Chain[G]]

  def union(rhs: Grp[G]): Grp[G] =
    if (rhs.order > lhs.order) rhs.union(lhs)
    else if (hasSubgroup(rhs)) lhs
    else if (rhs.generators.forall(pRep.represents(_))) {
      PGrpChain.unionByAdding(pRep, chain, rhs.generators)
    } else {
      val rhsPRep = rhs match {
        case pRhs: PGrp[G] => pRhs.pRep
        case _ => builder.defaultRepBuilder.build(rhs.generators)
      }
      val newRep = builder.defaultRepBuilder.genJoin(
        lhs.pRep, lhs.generators,
        rhsPRep, rhs.generators)
      PGrpChain.unionByAdding(pRep, builder.fromGrpIn(pRep)(lhs).chain, rhs.generators)
    }

  def intersect(rhs: Grp[G]): Grp[G] =
    if (rhs.order > lhs.order) rhs.intersect(lhs) // ensure lhs.order >= rhs.order
    else if (hasSubgroup(rhs)) rhs
    else if (rhs.generators.forall(pRep.represents(_))) {
      val newRhsChain = builder.fromGrpIn(pRep)(rhs).chain
      subgroupFor(Intersection(pRep.permutationAction, newRhsChain))
    } else {
      val rhsPRep = rhs match {
        case pRhs: PGrp[G] => pRhs.pRep
        case _ => builder.defaultRepBuilder.build(rhs.generators)
      }
      val newRep = builder.defaultRepBuilder.genJoin(
        lhs.pRep, lhs.generators,
        rhsPRep, rhs.generators)
      val newLhsGrp = builder.fromGrpIn(newRep)(lhs)
      val newRhsChain = builder.fromGrpIn(newRep)(rhs).chain
      newLhsGrp.subgroupFor(Intersection(newRep.permutationAction, newRhsChain)).copyWithParentOrNull(this)
    }

  def leftCosetsBy(rhs: Grp.SubgroupOf[this.type, G]): LeftCosets[G] =
    new LeftCosets[G] {

      type GG = PGrpChain.this.type
      val grp: GG = PGrpChain.this
      val subgrp: Grp.SubgroupOf[GG, G] = rhs

      def iterator: Iterator[LeftCoset[G]] = {
        val bo = BaseOrder(pRep.permutationAction, grp.chain.base)
        def rec(g: G, chain: Chain[G], subSubgrp: Grp[G]): Iterator[LeftCoset[G]] = chain match {
          case node: Node[G] =>
            for {
              b <- node.orbit.iterator
              bg = pRep.permutationAction.actr(b, g)
              (nextSubSubGrp, transversal) = subSubgrp.in(pRep).stabilizerTransversal(bg) if transversal.orbit.min(Order.ordering(bo)) == bg
              nextG = node.u(b) |+| g
              element <- rec(nextG, node.next, nextSubSubGrp)
            } yield element
          case _: Term[G] =>
            assert(subSubgrp.order == 1)
            Iterator(new LeftCoset(g, subgrp))
        }
        rec(Group[G].id, grp.chain, subgrp)
      }

    }

  def rightCosetsBy(rhs: Grp.SubgroupOf[this.type, G]): RightCosets[G] =
    new RightCosets[G] {
      type GG = PGrpChain.this.type
      val grp: GG = PGrpChain.this
      val subgrp: Grp.SubgroupOf[GG, G] = rhs
      def iterator = grp.leftCosetsBy(subgrp).iterator.map(_.inverse)
    }

  def fixingPartition(partition: Partition): Grp[G] =
    subgroupFor(FixingPartition(pRep.permutationAction, partition))

  def setwiseStabilizer(set: Set[Int]): Grp[G] =
    subgroupFor(SetwiseStabilizer(pRep.permutationAction, set))

  def find[Q:Eq:Permutation](q: Q): Opt[G] = chain.siftOther(q)

  /** Returns the subgroup for which `predicate` is satisfied; 
    * the test `backtrackTest` is used to prune the search tree.
    *
    * @param backtrackTest Tests if a pair (preimage, image) is valid 
    * for an element of the subgroup. 
    * False positives are allowed, but a false negative leads to incorrect prunes.
    * @param predicate Tests if an element is member of the subgroup
    * @return the subgroup satisfying `predicate`
    */

  def subgroupFor(backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): Grp[G] =
    subgroupFor(SubgroupDefinition(pRep.permutationAction, backtrackTest, predicate))

  def simplified: Grp[G] = {
    val mut = imply(pRep.permutationAction) { chain.mutableChain }
    mut.makeFullyMutable()
    mut.removeRedundantGenerators()
    val newChain = mut.toChain()
    val newGenerators = if (generators.size < newChain.strongGeneratingSet.size)
      generators
    else
      newChain.strongGeneratingSet
    new PGrpExplicit[Null, R, G](pRep, newChain, Opt(newGenerators)) // TODO: more generic
  }

}

object PGrpChain {

  type In[R0 <: FaithfulPRep[G] with Singleton, G] = PGrpChain[G] { type R = R0 }

  def unionByAdding[G:ClassTag:Eq:Group:PGrpChainBuilder](pRep: FaithfulPRep[G], chain: Chain[G], generatorsToAdd: Iterable[G])(implicit baseChange: BaseChange, schreierSims: SchreierSims): Grp[G] = {
    val mutableChain = BuildMutableChain.fromChain(chain, pRep.permutationAction)
    mutableChain.insertGenerators(generatorsToAdd)
    mutableChain.completeStrongGenerators()
    val newChain: Chain[G] = mutableChain.toChain()
    new PGrpExplicit[Null, pRep.type, G](pRep, newChain)
  }

}
