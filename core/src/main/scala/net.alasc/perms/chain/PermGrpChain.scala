package net.alasc.perms
package chain

import scala.annotation.tailrec

import spire.algebra.Group
import spire.syntax.action._
import spire.syntax.group._

import net.alasc.prep.bsgs
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, Permutation}
import net.alasc.prep.bsgs._

abstract class PermGrpChain[G] extends PermGrp[G] { lhs =>

  implicit val builder: PermGrpChainBuilder[G]

  implicit def permutation = builder.permutation

  import builder.{baseChange, classTag, schreierSims}

  def chain: bsgs.Chain[G]

  def chainOpt: Opt[bsgs.Chain[G]]

  def find[Q:Permutation](q: Q): Opt[G] = chain.siftOther(q)

  // enumeration of subgroup elements
  def lexElements: BigIndexedSeq[G] = new BigIndexedSeq[G] {
    val sizeOfDomain = largestMovedPoint.getOrElseFast(-1) + 1
    val lexChain = BuildChain.fromChain(chain, permutation, Opt(BaseGuideLex(sizeOfDomain)))
    def length = lhs.order
    def contains(g: G) = lhs.contains(g)
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

}
