package net.alasc.bsgs

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.action._

import net.alasc.algebra._

final class ChainCheck[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton] extends Check[Chain[G, F]] {
  import Check._

  def checkBaseAndStrongGeneratingSet(chain: Chain[G, F]): Checked = chain match {
    case node: Node[G, F] =>
      implicit def action: F = node.action
//      TODO
/*      val alg = algorithms.BasicAlgorithms.deterministic[P] // use deterministic algorithms to avoid looping forever on bad data
      val reconstructedChain = alg.completeChainFromGenerators(chain.strongGeneratingSet, chain.base)
      val reconstructedOrbits = reconstructedChain.start.next.nodesIterator.toSeq.map(_.orbitSize)
      val originalOrbits = node.nodesIterator.toSeq.map(_.orbitSize)
 Check.equals(originalOrbits, reconstructedOrbits, "Orbit sizes")*/
      Check.success
    case _ => Check.success
  }

  def checkOwnGenerators(chain: Chain[G, F]): Checked = {
    val baseSoFar = mutable.ArrayBuffer.empty[Int]
    @tailrec def rec(currentChecked: Checked, current: Chain[G, F], checkImmutable: Boolean): Checked = current match {
      case node: Node[G, F] =>
        implicit def action = node.action
        val fixingBase =
          node.ownGenerators.toList.flatMap(g => baseSoFar.flatMap(b => Check.equals(b <|+| g, b, s"Generator $g should fix")))
        val ownGeneratorsMoveBase =
          node.ownGenerators.toList.flatMap(g => Check.notEquals(node.beta <|+| g, node.beta, s"Generator $g should not fix"))
        baseSoFar += node.beta
        val immutableOk =
          if (checkImmutable) Check.equals(node.isImmutable, true, "All nodes after immutable node should be immutable") else Check.success
        rec(currentChecked ++ fixingBase ++ ownGeneratorsMoveBase ++ immutableOk, node.next, node.isImmutable)
      case _: Term[G, F] => currentChecked
    }
    rec(Check.success, chain, false)
  }

  def check(chain: Chain[G, F]): Checked =
    checkBaseAndStrongGeneratingSet(chain) ++ checkOwnGenerators(chain)
}
