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

/** Defines a subgroup (in the sense of the coset group obtained from the action kernel). */
trait SubgroupDefinition[G, A <: PermutationAction[G] with Singleton] {

  implicit def action: A

  def baseGuideOpt: Opt[BaseGuide]

  /** Returns whether the coset described by the element `g` is in the group. */
  def inSubgroup(g: G): Boolean

  /** Returns the test for the first level of `guidedChain`.
    * `guidedChain` must be using `action` and have a
    * base guided by `baseGuideOpt`.
    */
  def firstLevelTest(guidedChain: Chain[G, A]): SubgroupTest[G, A]

}

object SubgroupDefinition {

  class Simple[G, A <: PermutationAction[G] with Singleton]
    (backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean)(implicit val action: A) extends SubgroupDefinition[G, A] {

    def inSubgroup(g: G) = predicate(g)

    def baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide]

    object Test extends SubgroupTest[G, A] {
      def test(b: Int, orbitImage: Int, currentG: G, node: Node[G, A]): Opt[SubgroupTest[G, A]] =
        if (backtrackTest(node.beta, orbitImage)) Opt(this) else Opt.empty
    }

    def firstLevelTest(guidedChain: Chain[G, A]) = Test

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

  def apply[G, A <: PermutationAction[G] with Singleton]
    (backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean)(implicit action: A): SubgroupDefinition[G, A] =
    new Simple[G, A](backtrackTest, predicate)

}


trait SubgroupTest[G, A <: PermutationAction[G] with Singleton] {

  def test(b: Int, orbitImage: Int, currentG: G, node: Node[G, A]): Opt[SubgroupTest[G, A]]

}

object SubgroupSearch {

  /** Iterates through the subgroup described by the given definition. If the action is not faithful, the iteration
    * is done in the sense of coset elements.
    * @param definition  Subgroup definition
    * @param guidedChain BSGS chain whose base is guided by the subgroup definition
    * @return An iterator through the subgroup elements (or their coset representatives if the action is not faithful)
    */
  def generalSearch[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
    (definition: SubgroupDefinition[G, A], guidedChain: Chain[G, A]): Iterator[G] = {
    import definition.action
    val bo = BaseOrder[G, A](guidedChain.base)
    val firstTest = definition.firstLevelTest(guidedChain)
    def rec(currentChain: Chain[G, A], currentG: G, currentTest: SubgroupTest[G, A]): Iterator[G] = currentChain match {
      case node: Node[G, A] =>
        val sortedOrbit = node.orbit.toSeq.sorted(Order.ordering(ImageOrder(bo, currentG)))
        for {
          b <- sortedOrbit.iterator
          orbitImage = b <|+| currentG
          newTestOpt = currentTest.test(b, orbitImage, currentG, node) if newTestOpt.nonEmpty
          newTest = newTestOpt.get
          newG = node.u(b) |+| currentG
          g <- rec(node.next, newG, newTest)
        } yield g
      case _: Term[G, A] =>
        if (definition.inSubgroup(currentG)) Iterator(currentG) else Iterator.empty
    }
    rec(guidedChain, Group[G].id, firstTest)
  }

  def subgroupSearch[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
    (definition: SubgroupDefinition[G, A], guidedChain: Chain[G, A], kernel: Chain.Generic[G]): MutableChain[G, A] = {
    implicit def action: A = definition.action
    val bo = BaseOrder[G, A](guidedChain.base)
    val orbits = guidedChain.nodesIterator.map(_.orbit.toArray).toArray
    val length = orbits.length
    val subgroupChain = MutableChain.emptyWithBase[G, A](guidedChain.base)
    val firstTest = definition.firstLevelTest(guidedChain)
    // Tuple2Int contains (restartFrom, levelCompleted)
    def rec(level: Int, levelCompleted: Int, currentChain: Chain[G, A], currentSubgroup: Chain[G, A], currentG: G, currentTest: SubgroupTest[G, A]): Tuple2Int = currentChain match {
      case _: Term[G, A] =>
        if (definition.inSubgroup(currentG) && !kernel.siftsFaithful(currentG)) {
          subgroupChain.insertGenerators(Iterable(currentG))
          Tuple2Int(levelCompleted - 1, levelCompleted)
        } else
          Tuple2Int(level - 1, levelCompleted)
      case node: Node[G, A] =>
        val IsMutableNode(subgroupNode) = currentSubgroup
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
  def basePointGroups[G, A <: PermutationAction[G] with Singleton](chain: Chain[G, A], domainSize: Int): Array[Array[Int]] = {
    val remaining = metal.mutable.FixedBitSet.zeroUntil(domainSize)
    val groups = metal.mutable.Buffer.empty[Array[Int]]
    @tailrec def rec(current: Chain[G, A]): Array[Array[Int]] = current match {
      case node: Node[G, A] =>
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
      case _: Term[G, A] => groups.toArray
    }
    rec(chain)
  }

}
