package net.alasc.bsgs

import spire.algebra.Group
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util.Opt
import net.alasc.algebra.PermutationAction
import net.alasc.partitions.Partition

import scala.annotation.tailrec

case class PartitionStabilizer[G:Group, A <: PermutationAction[G] with Singleton]
  (partition: Partition)(implicit val action: A) extends SubgroupDefinition[G, A] {
  val n = partition.size

  @inline def block(p: Int): Int = if (p < n) partition.indexArray(p) else -1

  def inSubgroup(g: G): Boolean = PartitionStabilizer.partitionInvariantUnder(partition, action, g)

  def baseGuideOpt = Opt(BaseGuidePartition(partition))

  class Test(level: Int, pointSetsToTest: Array[Array[Int]], blockImages: Map[Int, Int]) extends SubgroupTest[G, A] {

    def test(b: Int, orbitImage: Int, currentG: G, node: Node[G, A]): Opt[Test] = {
      var newBlockImages = blockImages
      val pointSet = pointSetsToTest(level)
      val p0b = block(pointSet(0))
      val i0b = block(orbitImage)
      if (newBlockImages.isDefinedAt(p0b)) {
        if (newBlockImages(p0b) != i0b) return Opt.empty[Test]
      } else {
        newBlockImages += (p0b -> i0b)
      }
      if (pointSet.length > 1) {
        val nextG = node.u(b) |+| currentG // TODO: do not compute, compose actions
        @tailrec def checkBlock(i: Int): Boolean =
          if (i == pointSet.length) true else {
            val p = pointSet(i)
            val pb = block(p)
            val ib = block(action.actr(p, nextG))
            if (newBlockImages.isDefinedAt(pb)) {
              if (newBlockImages(pb) != ib)
                return false
            } else {
              newBlockImages += (pb -> ib)
            }
            checkBlock(i + 1)
          }
        if (!checkBlock(1)) return Opt.empty[Test]
      }
      Opt(new Test(level + 1, pointSetsToTest, newBlockImages))
    }
  }

  def firstLevelTest(guidedChain: Chain[G, A]): Test = {
    val pointSetsToTest: Array[Array[Int]] =
      SubgroupSearch.basePointGroups(guidedChain, n)
    new Test(0, pointSetsToTest, Map.empty)
  }

}

object PartitionStabilizer {

  def partitionInvariantUnder[G](partition: Partition, action: PermutationAction[G], g: G): Boolean = {
    @inline def block(p: Int): Int = if (p < partition.size) partition.indexArray(p) else -1
    cforRange(0 until partition.nBlocks) { b =>
      val p0 = partition.startArray(b)
      val ib = block(action.actr(p0, g))
      @tailrec def checkInBlock(p: Int): Boolean =
        if (p == -1) true
        else if (block(action.actr(p, g)) != ib) false
        else checkInBlock(partition.linkArray(p))
      if (!checkInBlock(partition.linkArray(p0)))
        return false
    }
    true
  }

}
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