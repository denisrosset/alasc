package net.alasc.math
package bsgs

import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.reflect.ClassTag

import scala.collection
import scala.collection.immutable
import scala.collection.mutable
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.monoid._

final class ChainCheck[P:ClassTag:Eq:Group] extends Check[Chain[P]] {
  import Check._

  def checkBaseAndStrongGeneratingSet(chain: Chain[P]): Checked = chain match {
    case node: Node[P] =>
      implicit def action = node.action
      val alg = algorithms.BasicAlgorithms.deterministic[P] // use deterministic algorithms to avoid looping forever on bad data
      val reconstructedChain = alg.completeChainFromGenerators(chain.strongGeneratingSet, chain.base)
      val reconstructedOrbits = reconstructedChain.start.next.nodesNext.map(_.orbitSize)
      val originalOrbits = node.nodesNext.map(_.orbitSize)
      Check.equals(originalOrbits, reconstructedOrbits, "Orbit sizes")
    case _ => Check.success
  }

  def checkOwnGenerators(chain: Chain[P]): Checked = {
    val baseSoFar = mutable.ArrayBuffer.empty[Int]
    @tailrec def rec(currentChecked: Checked, current: Chain[P], checkImmutable: Boolean): Checked = current match {
      case node: Node[P] =>
        implicit def action = node.action
        val fixingBase =
          node.ownGenerators.toList.flatMap(g => baseSoFar.flatMap(b => Check.equals(b <|+| g, b, s"Generator $g should fix")))
        val ownGeneratorsMoveBase =
          node.ownGenerators.toList.flatMap(g => Check.notEquals(node.beta <|+| g, node.beta, s"Generator $g should not fix"))
        baseSoFar += node.beta
        val immutableOk =
          if (checkImmutable) Check.equals(node.isImmutable, true, "All nodes after immutable node should be immutable") else Check.success
        rec(currentChecked ++ fixingBase ++ ownGeneratorsMoveBase ++ immutableOk, node.next, node.isImmutable)
      case _: Term[P] => currentChecked
    }
    rec(Check.success, chain, false)
  }

  def check(chain: Chain[P]): Checked =
    checkBaseAndStrongGeneratingSet(chain) ++ checkOwnGenerators(chain)
}
