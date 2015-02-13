package net.alasc.math
package bsgs
package algorithms

import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.{FaithfulPermutationAction, FiniteGroup}
import net.alasc.math.guide.BaseGuidePartition
import net.alasc.util._

object FixingPartition {
  // TODO: change pointSetsToTest to bitsets
  class FixingTest[P](level: Int, partition: Domain#Partition, pointSetsToTest: Array[Array[Int]])(implicit algebra: FiniteGroup[P]) extends SubgroupTest[P] {
    def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): Opt[FixingTest[P]] = {
      val pointSet = pointSetsToTest(level)
      if (partition.representative(pointSet(0)) != partition.representative(orbitImage))
        return Opt.empty[FixingTest[P]]
      if (pointSet.length > 1) {
        val nextG = node.u(b) |+| currentG
        var i = 1
        while (i < pointSet.length) {
          val k = pointSet(i)
          if (partition.representative(k) != partition.representative(k <|+| nextG))
            return Opt.empty[FixingTest[P]]
          i += 1
        }
      }
      Opt(new FixingTest[P](level + 1, partition, pointSetsToTest))
    }
  }
  def baseGuide[P](partition: Domain#Partition) = BaseGuidePartition(partition)
  def fixingPartition[P](chainWithGuidedBase: Chain[P], partition: Domain#Partition)(implicit algebra: FiniteGroup[P], alg: BasicAlgorithms[P]): Chain[P] = chainWithGuidedBase match {
    case node: Node[P] =>
      implicit def action = node.action
      val n = partition.size
      val pointSetsToTest: Array[Array[Int]] = alg.basePointGroups(chainWithGuidedBase, n)
      def leaveInvariant(g: P): Boolean = {
        var i = 0
        while (i < n) {
          if (partition.representative(i <|+| g) != partition.representative(i))
            return false
          i += 1
        }
        true
      }
      alg.subgroupSearch(chainWithGuidedBase, leaveInvariant(_), new FixingTest(0, partition, pointSetsToTest)).toChain
    case term: Term[P] => term
  }
}
