package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group, Order}
import spire.math.Sorting
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import metal.syntax._

import net.alasc.algebra.PermutationAction
import net.alasc.util._

trait SubgroupDefinition[G, F <: PermutationAction[G] with Singleton] {

  implicit def action: F

  def baseGuideOpt: Opt[BaseGuide]

  def inSubgroup(g: G): Boolean

  /** Returns the test for the first level of `guidedChain`.
    * `guidedChain` must be using `action` and have a
    * base guided by `baseGuideOpt`.
    */
  def firstLevelTest(guidedChain: Chain[G, F]): SubgroupTest[G, F]

}

object SubgroupDefinition {

  class Simple[G, F <: PermutationAction[G] with Singleton]
    (backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean)(implicit val action: F) extends SubgroupDefinition[G, F] {

    def inSubgroup(g: G) = predicate(g)

    def baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide]

    object Test extends SubgroupTest[G, F] {
      def test(b: Int, orbitImage: Int, currentG: G, node: Node[G, F]): Opt[SubgroupTest[G, F]] =
        if (backtrackTest(node.beta, orbitImage)) Opt(this) else Opt.empty
    }

    def firstLevelTest(guidedChain: Chain[G, F]) = Test

  }

  /** Returns the subgroup definition for which `predicate` is satisfied;
    * the test `backtrackTest` is used to prune the search tree.
    *
    * @param action Representation to use for `backtrackTest`
    * @param backtrackTest Tests if a pair (preimage, image) is valid for an element 
    *                      of the subgroup. False positives are allowed, but a false 
    *                      negative would incorrectly prune the tree.
    * @param predicate Tests if an element is member of the subgroup
    * @return the subgroup definition.
    */

  def apply[G, F <: PermutationAction[G] with Singleton]
    (backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean)(implicit action: F): SubgroupDefinition[G, F] =
    new Simple[G, F](backtrackTest, predicate)

}


trait SubgroupTest[G, F <: PermutationAction[G] with Singleton] {

  def test(b: Int, orbitImage: Int, currentG: G, node: Node[G, F]): Opt[SubgroupTest[G, F]]

}



object SubgroupSearch {

  def generalSearch[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (definition: SubgroupDefinition[G, F], guidedChain: Chain[G, F]): Iterator[G] = {
    import definition.action
    val bo = BaseOrder[G, F](guidedChain.base)
    val firstTest = definition.firstLevelTest(guidedChain)
    def rec(currentChain: Chain[G, F], currentG: G, currentTest: SubgroupTest[G, F]): Iterator[G] = currentChain match {
      case node: Node[G, F] =>
        val sortedOrbit = node.orbit.toSeq.sorted(Order.ordering(ImageOrder(bo, currentG)))
        for {
          b <- sortedOrbit.iterator
          orbitImage = b <|+| currentG
          newTestOpt = currentTest.test(b, orbitImage, currentG, node) if newTestOpt.nonEmpty
          newTest = newTestOpt.get
          newG = node.u(b) |+| currentG
          g <- rec(node.next, newG, newTest)
        } yield g
      case _: Term[G, F] =>
        if (definition.inSubgroup(currentG)) Iterator(currentG) else Iterator.empty
    }
    rec(guidedChain, Group[G].id, firstTest)
  }

  def subgroupSearch[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (definition: SubgroupDefinition[G, F], guidedChain: Chain[G, F]): MutableChain[G, F] = {
    implicit def action: F = definition.action
    val bo = BaseOrder[G, F](guidedChain.base)
    val orbits = guidedChain.nodesIterator.map(_.orbit.toArray).toArray
    val length = orbits.length
    val subgroupChain = MutableChain.emptyWithBase[G, F](guidedChain.base)
    val firstTest = definition.firstLevelTest(guidedChain)
    // Tuple2Int contains (restartFrom, levelCompleted)
    def rec(level: Int, levelCompleted: Int, currentChain: Chain[G, F], currentSubgroup: Chain[G, F], currentG: G, currentTest: SubgroupTest[G, F]): Tuple2Int = (currentChain, currentSubgroup) match {
      // TODO: remove tuple allocation
      case (_: Term[G, F], _) =>
        if (definition.inSubgroup(currentG) && !currentG.isId) {
          subgroupChain.insertGenerators(Iterable(currentG))
          Tuple2Int(levelCompleted - 1, levelCompleted)
        } else
          Tuple2Int(level - 1, levelCompleted)
      case (node: Node[G, F], IsMutableNode(subgroupNode)) =>
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
    val Tuple2Int(restartFrom, levelCompleted) = rec(0, length, guidedChain, subgroupChain.start.next, Group[G].id, firstTest)
    assert(levelCompleted == 0)
    subgroupChain.cutRedundantAfter(subgroupChain.start)
    subgroupChain
  }

  // TODO: consider returning array of additional domain points, not including the base point itself, or could even be changed to bitset, because the position of the base point is not important anymore
  /** Finds for each base point the additional domain points that are stabilized (i.e. are
    * not moved by the next subgroup in the stabilizer chain. The first element of each group
    * is the original base point.
    * 
    * The considered domain is `0 ... domainSize - 1`.
    */
  def basePointGroups[G, F <: PermutationAction[G] with Singleton](chain: Chain[G, F], domainSize: Int): Array[Array[Int]] = {
    val remaining = metal.mutable.BitSet((0 until domainSize): _*)
    val groups = metal.mutable.Buffer.empty[Array[Int]]
    @tailrec def rec(current: Chain[G, F]): Array[Array[Int]] = current match {
      case node: Node[G, F] =>
        import node.action
        val fixed = metal.mutable.Buffer[Int](node.beta)
        remaining -= node.beta
        remaining.foreach { k =>
          if (node.next.strongGeneratingSet.forall( g => (k <|+| g) == k))
            fixed += k
        }
        val array = fixed.toArray
        fixed.foreach { k => remaining -= k }
        groups += array
        rec(node.next)
      case _: Term[G, F] => groups.toArray
    }
    rec(chain)
  }

}
