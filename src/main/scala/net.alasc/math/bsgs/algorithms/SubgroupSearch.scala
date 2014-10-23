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
}

trait SubgroupSearchImpl[P] extends SchreierSims[P] with BaseChange[P] with BaseAlgorithms[P] with ChainBuilder[P] {
  self =>
  def generalSearch(givenChain: Chain[P], predicate: P => Boolean, givenTest: SubgroupTest[P])(
    implicit action: FaithfulPermutationAction[P]) : Iterator[P] = {
    val chain = withAction(givenChain, action)
    val bo = BaseOrder[P](chain.base)
    def rec(currentChain: Chain[P], currentG: P, currentTest: SubgroupTest[P]): Iterator[P] = currentChain match {
      case node: Node[P] =>
        val sortedOrbit = node.orbit.toSeq.sorted(Order.ordering(ImageOrder(bo, currentG)))
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
    val bo = BaseOrder[P](chain.base)
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
        Sorting.sort(orbit)(ImageOrder(bo, currentG), implicitly[ClassTag[Int]])
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
    cutRedundantAfter(subgroupChain, subgroupChain.start)
    subgroupChain
  }

  def basePointGroups(chain: Chain[P], domainSize: Int): Array[Array[Int]] = {
    val remaining = MutableBitSet((0 until domainSize): _*)
    val groups = debox.Buffer.empty[Array[Int]]
    @tailrec def rec(current: Chain[P]): Array[Array[Int]] = current match {
      case node: Node[P] =>
        import node.action
        val fixed = debox.Buffer[Int](node.beta)
        remaining -= node.beta
        remaining.foreachFast { k =>
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
}
