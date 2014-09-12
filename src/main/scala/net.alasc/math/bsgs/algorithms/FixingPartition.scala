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

object FixingPartition {
  // TODO: change pointSetsToTest to bitsets
  class FixingTest[P](level: Int, partition: Domain#Partition, pointSetsToTest: Array[Array[Int]])(implicit algebra: FiniteGroup[P]) extends SubgroupTest[P] {
    def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): RefOption[FixingTest[P]] = {
      val pointSet = pointSetsToTest(level)
      if (partition.representative(pointSet(0)) != partition.representative(orbitImage))
        return RefNone
      if (pointSet.length > 1) {
        val nextG = node.u(b) |+| currentG
        var i = 1
        while (i < pointSet.length) {
          val k = pointSet(i)
          if (partition.representative(k) != partition.representative(k <|+| nextG))
            return RefNone
          i += 1
        }
      }
      RefSome(new FixingTest[P](level + 1, partition, pointSetsToTest))
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
