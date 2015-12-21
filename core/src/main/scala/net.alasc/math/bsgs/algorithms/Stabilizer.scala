package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable

import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.{FaithfulPermutationAction, FiniteGroup}
import net.alasc.math.guide.BaseGuideSet
import net.alasc.util._

object Stabilizer {
  def baseGuide(set: Set[Int]) = BaseGuideSet(set)
  def pointwiseStabilizer[P](chainWithGuidedBase: Chain[P], set: Set[Int])(implicit algebra: FiniteGroup[P], alg: BasicAlgorithms[P]): Chain[P] = chainWithGuidedBase match {
    case node: Node[P] =>
      @tailrec def firstNotInSet(current: Chain[P]): Chain[P] = current match {
        case currentNode: Node[P] if set.contains(currentNode.beta) => firstNotInSet(currentNode.next)
        case _ => current
      }
      val res = firstNotInSet(chainWithGuidedBase)
      assert(set.forall(res.isFixed(_)))
      res
    case term: Term[P] => term
  }
  class FixingTest[P](level: Int, set: Set[Int], pointSetsToTest: Array[BitSet]) extends SubgroupTest[P] {
    def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): Opt[FixingTest[P]] =
      if (level < pointSetsToTest.length) {
        if (!set.contains(orbitImage))
          return Opt.empty[FixingTest[P]]
        val pointSet = pointSetsToTest(level)
        if (pointSet.size > 1) {
          val nodeU = node.u(b)
          if (pointSet.exists( k => k != node.beta && !set.contains((k <|+| nodeU) <|+| currentG) ))
            return Opt.empty[FixingTest[P]]
        }
        Opt(new FixingTest(level + 1, set, pointSetsToTest))
      } else {
        Opt(this)
      }
  }
  def setwiseStabilizer[P](chainWithGuidedBase: Chain[P], set: Set[Int])(implicit algebra: FiniteGroup[P], alg: BasicAlgorithms[P]): Chain[P] = chainWithGuidedBase match {
    case node: Node[P] =>
      implicit def action = node.action
      // Finds for each base point the additional points that are stabilized (i.e. are
      // not moved by the next subgroup in the stabilizer chain.
      // The points considered are those contained in `set`.
      val pointSetsToTest: Array[BitSet] = {
        val remaining = MutableBitSet.empty ++= set
        val groups = metal.Buffer.empty[BitSet]
        @tailrec def rec(current: Chain[P]): Array[BitSet] = current match {
          case node: Node[P] if remaining.contains(node.beta) =>
            val fixed = mutable.BitSet(node.beta)
            remaining -= node.beta
            remaining.foreachFast { k =>
              if (node.next.isFixed(k)) fixed += k
            }
            remaining --= fixed
            groups += fixed.toImmutable
            rec(node.next)
          case _ => groups.toArray
        }
        rec(chainWithGuidedBase)
      }
      def setwiseStabilized(g: P): Boolean =
        set.forall { k => set.contains(k <|+| g) }
      alg.subgroupSearch(chainWithGuidedBase, setwiseStabilized, new FixingTest(0, set, pointSetsToTest)).toChain
    case term: Term[P] => term
  }
}
