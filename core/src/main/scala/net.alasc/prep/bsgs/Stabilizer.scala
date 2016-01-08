package net.alasc.prep.bsgs

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.util._

case class SetwiseStabilizer[G:Group](val action: FaithfulPermutationAction[G], val set: Set[Int]) extends SubgroupDefinition[G] {

  def inSubgroup(g: G): Boolean =
    set.forall { k => set.contains(action.actr(k, g)) }

  def baseGuideOpt = Opt(BaseGuideSet(set))

  class Test(level: Int, pointSetsToTest: Array[BitSet]) extends SubgroupTest[G] {

    def test(b: Int, orbitImage: Int, currentG: G, node: Node[G]): Opt[Test] =
      if (level < pointSetsToTest.length) {
        if (!set.contains(orbitImage))
          return Opt.empty[Test]
        val pointSet = pointSetsToTest(level)
        if (pointSet.size > 1) {
          val nodeU: G = node.u(b)
          if (pointSet.exists( k => k != node.beta &&
            !set.contains(action.actr(action.actr(k, nodeU), currentG)) ))
            return Opt.empty[Test]
        }
        Opt(new Test(level + 1, pointSetsToTest))
      } else
        Opt(this)
  }

  def firstLevelTest(guidedChain: Chain[G]): Test = {
    // Finds for each base point the additional points that are stabilized (i.e. are
    // not moved by the next subgroup in the stabilizer chain.
    // The points considered are those contained in `set`.
    val pointSetsToTest: Array[BitSet] = {
      val remaining = MutableBitSet.empty ++= set
      val groups = metal.Buffer.empty[BitSet]
      @tailrec def rec(current: Chain[G]): Array[BitSet] = current match {
        case node: Node[G] if remaining.contains(node.beta) =>
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
      rec(guidedChain)
    }
    new Test(0, pointSetsToTest)
  }

}

object PointwiseStabilizer {

  def baseGuide(set: Set[Int]) = BaseGuideSet(set)

  def recurse[G:ClassTag:Eq:Group](guidedChain: Chain[G], set: Set[Int]): Chain[G] =
    guidedChain match {
      case node: Node[G] =>
        @tailrec def firstNotInSet(current: Chain[G]): Chain[G] = current match {
          case currentNode: Node[G] if set.contains(currentNode.beta) => firstNotInSet(currentNode.next)
          case _ => current
        }
        val res = firstNotInSet(guidedChain)
        assert(set.forall(res.isFixed(_))) // TODO: remove
        res
      case term: Term[G] => term
    }

}
