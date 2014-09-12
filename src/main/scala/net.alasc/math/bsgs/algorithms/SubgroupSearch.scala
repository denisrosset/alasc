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

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.syntax.subgroup._
import net.alasc.util._

trait SubgroupTest[P] extends AnyRef {
  def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): RefOption[SubgroupTest[P]]
}

class TrivialSubgroupTest[P] extends SubgroupTest[P] {
  def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]) = RefSome(this)
}

trait SubgroupSearch[P] {
  def generalSearch(givenChain: Chain[P], predicate: P => Boolean, givenTest: SubgroupTest[P])(
    implicit action: FaithfulPermutationAction[P]): Iterator[P]

  def subgroupSearch(givenChain: Chain[P], predicate: P => Boolean, test: SubgroupTest[P])(
    implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  // TODO: consider returning array of additional domain points, not including the base point itself
  /** Finds for each base point the additional domain points that are stabilized (i.e. are
    * not moved by the next subgroup in the stabilizer chain. The first element of each group
    * is the original base point.
    * 
    * The considered domain is `0 ... domainSize - 1`.
    */
  def basePointGroups(chain: Chain[P], domainSize: Int): Array[Array[Int]]

  // TODO: remove action (not needed)
  def intersection(givenChain1: Chain[P], givenChain2: Chain[P])(implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def fixingPartition(givenChain: Chain[P], partition: Domain#Partition)(implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def pointwiseStabilizer(givenChain: Chain[P], points: Set[Int])(implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def setwiseStabilizer(givenChain: Chain[P], points: Set[Int])(implicit action: FaithfulPermutationAction[P]): MutableChain[P]
}

trait SubgroupSearchImpl[P] extends Orders[P] with SchreierSims[P] with BaseChange[P] with BaseAlgorithms[P] with ChainBuilder[P] {
  self =>
  def generalSearch(givenChain: Chain[P], predicate: P => Boolean, givenTest: SubgroupTest[P])(
    implicit action: FaithfulPermutationAction[P]) : Iterator[P] = {
    val chain = withAction(givenChain, action)
    val bo = baseOrder(chain.base)
    def rec(currentChain: Chain[P], currentG: P, currentTest: SubgroupTest[P]): Iterator[P] = currentChain match {
      case node: Node[P] =>
        val sortedOrbit = node.orbit.toSeq.sorted(Order.ordering(imageOrder(bo, currentG)))
        for {
          b <- sortedOrbit.iterator
          orbitImage = b <|+| currentG
          newTestOpt = currentTest.test(b, orbitImage, currentG, node) if newTestOpt.nonEmpty
          newTest = newTestOpt.get
          newG = node.u(b) |+| currentG
          g <- rec(node.next, newG, newTest)
        } yield g
      case _: Term[P] =>
        if (predicate(currentG)) Iterator(currentG) else Iterator.empty
    }
    rec(givenChain, algebra.id, givenTest)
  }

  def subgroupSearch(givenChain: Chain[P], predicate: P => Boolean, test: SubgroupTest[P])(
    implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val chain = withAction(givenChain, action)
    val bo = baseOrder(chain.base)
    val length = givenChain.nodesNext.size
    val orbits = givenChain.nodesNext.map(_.orbit.toArray).toArray
    val subgroupChain = emptyChainWithBase(givenChain.base)
    // Tuple2Int contains (restartFrom, levelCompleted)
    def rec(level: Int, levelCompleted: Int, currentChain: Chain[P], currentSubgroup: Chain[P], currentG: P, currentTest: SubgroupTest[P]): Tuple2Int = (currentChain, currentSubgroup) match {
      case (_: Term[P], _) =>
        if (predicate(currentG) && !currentG.isId) {
          insertGenerators(subgroupChain, Iterable(currentG))
          Tuple2Int(levelCompleted - 1, levelCompleted)
        } else
          Tuple2Int(level - 1, levelCompleted)
      case (node: Node[P], IsMutableNode(subgroupNode)) =>
        var newLevelCompleted = levelCompleted
        val orbit = orbits(level)
        Sorting.sort(orbit)(imageOrder(bo, currentG), implicitly[ClassTag[Int]])
        var sPrune = orbit.length
        var n = orbit.length
        var i = 0
        while (i < n) {
          val deltaP = orbit(i)
          val delta = deltaP <|+| currentG
          val newTestOpt = currentTest.test(deltaP, delta, currentG, node)
          if (newTestOpt.nonEmpty) {
            val newTest = newTestOpt.get
            val newG = node.u(deltaP) |+| currentG
            if (sPrune < subgroupNode.orbitSize)
              return Tuple2Int(level - 1, level)
            val Tuple2Int(subRestartFrom, subLevelCompleted) = rec(level + 1, newLevelCompleted, node.next, subgroupNode.next, newG, newTest)
            newLevelCompleted = subLevelCompleted
            if (subRestartFrom < level)
              return Tuple2Int(subRestartFrom, newLevelCompleted)
            sPrune -= 1
          }
          i += 1
        }
        Tuple2Int(level - 1, level)
      case _ => sys.error("Invalid argument")
    }
    val Tuple2Int(restartFrom, levelCompleted) = rec(0, length, givenChain, subgroupChain.start.next, algebra.id, test)
    assert(levelCompleted == 0)
    subgroupChain
  }

  def intersection(givenChain1: Chain[P], givenChain2: Chain[P])(implicit action: FaithfulPermutationAction[P]): MutableChain[P] =
    if (givenChain1.length < givenChain2.length)
      intersection(givenChain2, givenChain1)
    else {
      if (givenChain1.order == 1 || givenChain2.order == 1) return emptyChainWithBase(Seq.empty)
      val chain1 = withAction(givenChain1, action)
      val chain2 = withBase(givenChain2, chain1.base)(action)
      class IntersectionTest(level: Int, chain2: Chain[P], prev2Inv: P) extends SubgroupTest[P] {
        def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): RefOption[IntersectionTest] = {
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
      subgroupSearch(chain1, g => chain2.contains(g), new IntersectionTest(0, chain2, algebra.id))
    }

  def basePointGroups(chain: Chain[P], domainSize: Int): Array[Array[Int]] = {
    val remaining = mutable.BitSet((0 until domainSize): _*)
    val groups = debox.Buffer.empty[Array[Int]]
    @tailrec def rec(current: Chain[P]): Array[Array[Int]] = current match {
      case node: Node[P] =>
        import node.action
        val fixed = debox.Buffer[Int](node.beta)
        remaining -= node.beta
        remaining.foreach { k =>
          if (node.next.strongGeneratingSet.forall( g => (k <|+| g) == k))
            fixed += k
        }
        val array = fixed.toArray
        remaining --= array
        groups += array
        rec(node.next)
      case _: Term[P] => groups.toArray
    }
    rec(chain)
  }

  def pointwiseStabilizer(givenChain: Chain[P], set: Set[Int])(implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val mutableChain = mutableChainCopyWithBase(givenChain, BaseGuideSet(set))(action)
    @tailrec def detachFirstIfInSet: Unit = mutableChain.start.next match {
      case node: Node[P] if set.contains(node.beta) =>
        mutableChain.detachFirstNode(sys.error("Should never be called"))
        detachFirstIfInSet
      case node: Node[P] =>
        assert(set.forall(k => node.isFixed(k)))
      case _: Term[P] =>
    }
    detachFirstIfInSet
    mutableChain
  }

  def setwiseStabilizer(givenChain: Chain[P], set: Set[Int])(implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val reorderedChain = withBase(givenChain, BaseGuideSet(set))(action)
    /** Finds for each base point the additional points that are stabilized (i.e. are
      * not moved by the next subgroup in the stabilizer chain.
      * 
      * The points considered are those contained in `set`.
      */
    def basePointGroupsInSet: Array[BitSet] = {
      val remaining = mutable.BitSet.empty ++= set
      val groups = debox.Buffer.empty[BitSet]
      @tailrec @inline def rec(current: Chain[P]): Array[BitSet] = current match {
        case node: Node[P] if remaining.contains(node.beta) =>
          import node.action
          val fixed = mutable.BitSet(node.beta)
          remaining -= node.beta
          remaining.foreach { k =>
            if (node.next.isFixed(k)) fixed += k
          }
          remaining --= fixed
          groups += fixed.toImmutable
          rec(node.next)
        case _ => groups.toArray
      }
      rec(reorderedChain)
    }
    val pointSetsToTest: Array[BitSet] = basePointGroupsInSet
    class FixingTest(level: Int) extends SubgroupTest[P] {
      def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): RefOption[FixingTest] =
        if (level < pointSetsToTest.length) {
          if (!set.contains(orbitImage))
            return RefNone
          val pointSet = pointSetsToTest(level)
          if (pointSet.size > 1) {
            val nextG = node.u(b) |+| currentG
            if (pointSet.exists( k => k != node.beta && !set.contains(k <|+| nextG) ))
              return RefNone
          }
          RefSome(new FixingTest(level + 1))
        } else {
          RefSome(this)
        }
    }
    def setwiseStabilized(g: P): Boolean =
      set.forall { k => set.contains(k <|+| g) }
    subgroupSearch(reorderedChain, setwiseStabilized(_), new FixingTest(0))
  }
  def fixingPartition(givenChain: Chain[P], partition: Domain#Partition)(implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val n = partition.size
    val orderedPartition = partition.sizeIncreasing
    val reorderedChain = withBase(givenChain, PartitionGuide(partition))(action)
    val pointSetsToTest: Array[Array[Int]] = basePointGroups(reorderedChain, n)
    class FixingTest(level: Int) extends SubgroupTest[P] {
      def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): RefOption[FixingTest] = {
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
        RefSome(new FixingTest(level + 1))
      }
    }
    def leaveInvariant(g: P): Boolean = {
      var i = 0
      while (i < n) {
        if (partition.representative(i <|+| g) != partition.representative(i))
          return false
        i += 1
      }
      true
    }
    subgroupSearch(reorderedChain, leaveInvariant(_), new FixingTest(0))
  }
}
