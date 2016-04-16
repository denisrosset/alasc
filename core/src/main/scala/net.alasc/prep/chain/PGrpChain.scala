package net.alasc.prep
package chain

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationBuilder}
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

  def subgroupFor(definition: SubgroupDefinition[G]): Grp[G] = builder.subgroupFor(this, definition)

  def chain: Chain[G]

  def chainOpt: Opt[Chain[G]]

  def fixingPartition(partition: Partition): Grp[G] =
    subgroupFor(FixingPartition(pRep.permutationAction, partition))

  def setwiseStabilizer(set: Set[Int]): Grp[G] =
    subgroupFor(SetwiseStabilizer(pRep.permutationAction, set))

  def find[Q:Eq:PermutationBuilder](q: Q): Opt[G] = chain.siftOther(q)

}

object PGrpChain {

  type In[R0 <: FaithfulPRep[G] with Singleton, G] = PGrpChain[G] { type R = R0 }

}
