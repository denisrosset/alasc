package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.immutable.BitSet

import spire.algebra.Order
import spire.syntax.groupAction._
import spire.syntax.group._
import spire.math.Sorting

import net.alasc.algebra.{FaithfulPermutationAction, FiniteGroup, Subgroup}
import net.alasc.syntax.check._
import net.alasc.syntax.subgroup._
import net.alasc.util._

object Intersection {
  class IntersectionTest[P](level: Int, chain2: Chain[P], prev2Inv: P)(implicit algebra: FiniteGroup[P]) extends SubgroupTest[P] {
    def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): RefOption[IntersectionTest[P]] = {
      val b2 = orbitImage <|+| prev2Inv
      chain2 match {
        case node2: Node[P] if node2.inOrbit(b2) =>
          RefSome(new IntersectionTest(level + 1, node2.next, prev2Inv |+| node2.uInv(b2)))
        case _ if node.beta == b2 =>
          RefSome(new IntersectionTest(level + 1, chain2, prev2Inv))
        case _ =>
          RefNone
      }
    }
  }
  def baseGuide[P](chain1: Chain[P]) = BaseGuideSeq(chain1.base)
  def intersection[P](chain1: Chain[P], chain2WithGuidedBase: Chain[P])(implicit algebra: FiniteGroup[P], alg: BasicAlgorithms[P]): Chain[P] =
    chain1 match {
      case node1: Node[P] => chain2WithGuidedBase match {
        case node2: Node[P] =>
          implicit def action = node1.action
          assert(node1.base.startsWith(node2.base))
          assert(node1.action == node2.action)
          alg.subgroupSearch(chain1, g => chain2WithGuidedBase.contains(g), new IntersectionTest(0, chain2WithGuidedBase, algebra.id)).toChain
        case term2: Term[P] => term2
      }
      case term1: Term[P] => term1
    }
}
