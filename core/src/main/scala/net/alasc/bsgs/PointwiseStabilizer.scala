package net.alasc.bsgs

import net.alasc.algebra.PermutationAction
import spire.algebra.{Eq, Group}

import scala.annotation.tailrec
import scala.reflect.ClassTag

object PointwiseStabilizer {

  def baseGuide(set: Set[Int]) = BaseGuideSet(set)

  def recurse[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton](guidedChain: Chain[G, A], set: Set[Int]): Chain[G, A] =
    guidedChain match {
      case node: Node[G, A] =>
        @tailrec def firstNotInSet(current: Chain[G, A]): Chain[G, A] = current match {
          case currentNode: Node[G, A] if set.contains(currentNode.beta) => firstNotInSet(currentNode.next)
          case _ => current
        }
        val res = firstNotInSet(guidedChain)
        res
      case term: Term[G, A] => term
    }

}
