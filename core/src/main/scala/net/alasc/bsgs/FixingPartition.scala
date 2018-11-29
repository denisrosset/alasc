package net.alasc.bsgs

import spire.algebra.Group
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util.Opt
import net.alasc.algebra.PermutationAction
import net.alasc.partitions.Partition

import scala.annotation.tailrec




case class FixingPartition[G:Group, A <: PermutationAction[G] with Singleton]
  (partition: Partition)(implicit val action: A) extends SubgroupDefinition[G, A] {

  val n = partition.size

  def inSubgroup(g: G): Boolean = FixingPartition.partitionFixedUnder(partition, action, g)

  def baseGuideOpt = Opt(BaseGuidePartition(partition))

    // TODO: change pointSetsToTest to bitsets
  class Test(level: Int, pointSetsToTest: Array[Array[Int]]) extends SubgroupTest[G, A] {

    def test(b: Int, orbitImage: Int, currentG: G, node: Node[G, A]): Opt[Test] = {
      val pointSet = pointSetsToTest(level)
      if (partition.representative(pointSet(0)) != partition.representative(orbitImage))
        return Opt.empty[Test]
      if (pointSet.length > 1) {
        val nextG = node.u(b) |+| currentG // replace by successive action evaluation? TODO
        var i = 1
        while (i < pointSet.length) {
          val k = pointSet(i)
          if (partition.representative(k) != partition.representative(action.actr(k, nextG)))
            return Opt.empty[Test]
          i += 1
        }
      }
      Opt(new Test(level + 1, pointSetsToTest))
    }
  }

  def firstLevelTest(guidedChain: Chain[G, A]): Test = {
    val pointSetsToTest: Array[Array[Int]] =
      SubgroupSearch.basePointGroups(guidedChain, n)
    new Test(0, pointSetsToTest)
  }

}

object FixingPartition {

  @inline def partitionFixedUnder[G](partition: Partition, action: PermutationAction[G], g: G): Boolean = {
    cforRange(0 until partition.size) { i =>
      if (partition.representative(action.actr(i, g)) != partition.representative(i))
        return false
    }
    true
  }

}