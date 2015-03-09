package net.alasc
package math

import scala.annotation.tailrec
import scala.language.implicitConversions

import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.math.bsgs._
import net.alasc.math.guide.BaseGuideLex
import big._

class GrpLexElements[G](val lhs: Grp[G]) {
  import lhs._
  // enumeration of subgroup elements
  def lexElements(implicit rp: Representation[G]): BigIndexedSet[G] = new BigIndexedSet[G] {
    implicit val action = rp.action
    val lexChain = lhs.chain(rp, BaseGuideLex(rp.size))
    def size = lhs.order
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
