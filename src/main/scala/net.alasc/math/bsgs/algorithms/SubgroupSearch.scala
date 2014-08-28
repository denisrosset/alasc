package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.BitSet

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

  // TODO: remove action (not needed)
  def intersection(givenChain1: Chain[P], givenChain2: Chain[P])(implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def fixingSequence(givenChain: Chain[P], seq: Seq[Any])(implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def pointwiseStabilizer(givenChain: Chain[P], points: Set[Int])(implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def setwiseStabilizer(givenChain: Chain[P], points: Set[Int])(implicit action: FaithfulPermutationAction[P]): MutableChain[P]
}

trait SubgroupSearchImpl[P] extends Orders[P] with SchreierSims[P] with BaseChange[P] with BaseAlgorithms[P] {
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

  case class SubgroupSearchResult(restartFrom: Int, levelCompleted: Int)

  def subgroupSearch(givenChain: Chain[P], predicate: P => Boolean, test: SubgroupTest[P])(
    implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val chain = withAction(givenChain, action)
    val bo = baseOrder(chain.base)
    val length = givenChain.nodesNext.size
    val orbits = givenChain.nodesNext.map(_.orbit.toArray).toArray
    val subgroupChain = emptyChainWithBase(givenChain.base)
    def rec(level: Int, levelCompleted: Int, currentChain: Chain[P], currentSubgroup: Chain[P], currentG: P, currentTest: SubgroupTest[P]): SubgroupSearchResult = (currentChain, currentSubgroup) match {
      case (_: Term[P], _) =>
        if (predicate(currentG) && !currentG.isId) {
          insertGenerators(subgroupChain, Iterable(currentG))
          SubgroupSearchResult(levelCompleted - 1, levelCompleted)
        } else
          SubgroupSearchResult(level - 1, levelCompleted)
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
              return SubgroupSearchResult(level - 1, level)
            val SubgroupSearchResult(subRestartFrom, subLevelCompleted) = rec(level + 1, newLevelCompleted, node.next, subgroupNode.next, newG, newTest)
            newLevelCompleted = subLevelCompleted
            if (subRestartFrom < level)
              return SubgroupSearchResult(subRestartFrom, newLevelCompleted)
            sPrune -= 1
          }
          i += 1
        }
        SubgroupSearchResult(level - 1, level)
      case _ => sys.error("Invalid argument")
    }
    val SubgroupSearchResult(restartFrom, levelCompleted) = rec(0, length, givenChain, subgroupChain.start.next, algebra.id, test)
    assert(levelCompleted == 0)
    subgroupChain
  }

  def intersection(givenChain1: Chain[P], givenChain2: Chain[P])(implicit action: FaithfulPermutationAction[P]): MutableChain[P] =
    if (givenChain1.length < givenChain2.length)
      intersection(givenChain2, givenChain1)
    else {
      val chain1 = withAction(givenChain1, action)
      val chain2 = mutableCopyWithAction(givenChain2, action)
      changeBase(chain2, chain1.base)
      class IntersectionTest(level: Int, chain2: Chain[P], prev2Inv: P) extends SubgroupTest[P] {
        def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): RefOption[IntersectionTest] = {
          val b2 = orbitImage <|+| prev2Inv
          val node2 = chain2.asInstanceOf[Node[P]]
          if (node2.inOrbit(b2))
            RefSome(new IntersectionTest(level + 1, node2.next, prev2Inv |+| node2.uInv(b2)))
          else
            RefNone
        }
      }
      subgroupSearch(chain1, g => chain2.start.next.contains(g), new IntersectionTest(0, chain2.start.next, algebra.id))
    }

  /** Finds for each base point the additional domain points that are stabilized (i.e. are
    * not moved by the next subgroup in the stabilizer chain. The first element of each group
    * is the original base point.
    * 
    * The considered domain is `0 ... domainSize - 1`.
    */
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
    val mutableChain = mutableCopyWithAction(givenChain, action)
    changeBase(mutableChain, BaseGuideSet(set))
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
    val mutableChain = mutableCopyWithAction(givenChain, action)
    changeBase(mutableChain, BaseGuideSet(set))
    val reorderedChain = mutableChain.toChain
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
          groups += fixed
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

  def fixingSequence(givenChain: Chain[P], seq: Seq[Any])(implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val n = seq.size
    val partition = Partition.fromSeq(seq)
    val mutableChain = mutableCopyWithAction(givenChain, action)
    changeBase(mutableChain, partition.guide)
    val reorderedChain = mutableChain.toChain
    val pointSetsToTest: Array[Array[Int]] = basePointGroups(reorderedChain, n)
    val seqInteger = partition.blockIndex
    class FixingTest(level: Int) extends SubgroupTest[P] {
      def test(b: Int, orbitImage: Int, currentG: P, node: Node[P])(implicit action: FaithfulPermutationAction[P]): RefOption[FixingTest] = {
        val pointSet = pointSetsToTest(level)
        if (seqInteger(pointSet(0)) != seqInteger(orbitImage))
          return RefNone
        if (pointSet.length > 1) {
          val nextG = node.u(b) |+| currentG
          var i = 1
          while (i < pointSet.length) {
            val k = pointSet(i)
            if (seqInteger(k) != seqInteger(k <|+| nextG))
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
        if (seqInteger(i <|+| g) != seqInteger(i))
          return false
        i += 1
      }
      true
    }
    subgroupSearch(reorderedChain, leaveInvariant(_), new FixingTest(0))
  }
}
