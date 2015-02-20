package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec
import scala.util.Random

import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.subgroup._

trait SchreierSims[P] extends MutableAlgorithms[P] with AddGeneratorsAlgorithms[P] {
  def completeChainActionChange(oldChain: Chain[P], newAction: FaithfulPermutationAction[P], givenBase: Seq[Int] = Seq.empty): MutableChain[P] =
    completeChainFromSubgroup(oldChain, givenBase)(newAction, Chain.ChainSubgroup)

  def completeChainFromGenerators(generators: Iterable[P], givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def completeChainFromGeneratorsAndOrder(generators: Iterable[P], order: BigInt, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def completeChainFromGeneratorsRandomAndOrder(generators: Iterable[P], randomElement: Function1[Random, P], order: BigInt, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]): MutableChain[P]

  def completeChainFromSubgroup[S](s: S, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P], subgroup: Subgroup[S, P]): MutableChain[P]
}

trait SchreierSimsCommon[P] extends SchreierSims[P] with AddGeneratorsAlgorithms[P] {
  /** Finds if a new strong generator can be found at the given `node`, assuming that
    * the chain starting at `node.tail` is complete.
    * 
    * If a new strong generator is found, returns some pair containing
    * the node and the strong generator to insert there.
    */
  def findNewStrongGeneratorAt(mutableChain: MutableChain[P], node: Node[P]): Option[(MutableNode[P], P)] = {
    node.foreachOrbit { b =>
      implicit def action = node.action
      val ub = node.u(b)
      for (x <- node.strongGeneratingSet) {
        val i = b <|+| x
        if (!node.inOrbit(i))
          return Some(mutableChain.mutable(node) -> x)
        val ubx = ub |+| x
        if (ubx =!= node.u(i)) {
          val schreierGen = ubx |+| node.uInv(i)
          val res = siftAndUpdateBaseFrom(mutableChain, node, schreierGen)
          if (!res.isEmpty) return res.toOption
        }
      }
    }
    None
  }

  /** Completes the set of strong generators starting at `node`, assuming that `node.tail` is already completed.
    * 
    * Inspired (but rewritten) from SCHREIERSIMS, page 91 of Holt (2005).
    */
  @tailrec final def completeStrongGeneratorsAt(mutableChain: MutableChain[P], mutableNode: MutableNode[P]): Unit =
    findNewStrongGeneratorAt(mutableChain, mutableNode) match {
      case None => mutableNode.prev match {
        case IsMutableNode(mutablePrev) =>
          // current node does not have new strong generators, but node has parent that has to be completed
          completeStrongGeneratorsAt(mutableChain, mutablePrev)
        case node: Node[P] => sys.error("mutableNode.prev cannot be immutable")
        case start: Start[P] =>
          // current node does not have new strong generators, and current node starts the chain, we are finished
      }
      case Some((where, newGenerator)) =>
        // there is a new strong generator at the node `where`, add it there and restart the search there
        implicit def action = mutableChain.start.action
        addStrongGeneratorHere(mutableChain, where, newGenerator, newGenerator.inverse)
        completeStrongGeneratorsAt(mutableChain, where)
    }

  /** Deterministic Schreier-Sims algorithm. */
  def completeStrongGenerators(mutableChain: MutableChain[P]): Unit = mutableChain.findLast() match {
    case _: Start[P] => // chain is empty, no generators to find
    case node: Node[P] => completeStrongGeneratorsAt(mutableChain, mutableChain.mutable(node))
  }

  /** Deterministic Schreier-Sims algorithm. */
  def deterministicSchreierSims(generators: Iterable[P], givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val mutableChain = incompleteChainWithGenerators(generators, givenBase)
    completeStrongGenerators(mutableChain)
    mutableChain
  }
}

trait SchreierSimsDeterministic[P] extends SchreierSimsCommon[P] {
  def completeChainFromGenerators(generators: Iterable[P], givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]) = deterministicSchreierSims(generators, givenBase)

  def completeChainFromGeneratorsAndOrder(generators: Iterable[P], order: BigInt, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]) = {
    val mutableChain = deterministicSchreierSims(generators, givenBase)
    assert(mutableChain.start.next.order == order)
    mutableChain
  }

  def completeChainFromAnotherChain(chain: Chain[P], newAction: FaithfulPermutationAction[P], givenBase: Seq[Int] = Seq.empty) =
    completeChainFromGeneratorsAndOrder(chain.generators, chain.order, givenBase)(newAction)

  def completeChainFromGeneratorsRandomAndOrder(generators: Iterable[P], randomElement: Function1[Random, P], order: BigInt, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]) =
    completeChainFromGeneratorsAndOrder(generators, order, givenBase)

  def completeChainFromSubgroup[S](s: S, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P], subgroup: Subgroup[S, P]) =
    completeChainFromGeneratorsAndOrder(s.generators, s.order, givenBase)(action)
}

trait SchreierSimsRandomized[P] extends SchreierSimsCommon[P] with RandomizedAlgorithms {
  def completeChainFromGenerators(generators: Iterable[P], givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]) = deterministicSchreierSims(generators, givenBase)


  def completeChainFromGeneratorsAndOrder(generators: Iterable[P], order: BigInt, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]) =
    randomizedSchreierSims(RandomBag(generators, randomGenerator).randomElement(_), order, givenBase)

  def completeChainFromGeneratorsRandomAndOrder(generators: Iterable[P], randomElement: Function1[Random, P], order: BigInt, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P]) =
    randomizedSchreierSims(randomElement, order, givenBase)

  def completeChainFromAnotherChain(chain: Chain[P], newAction: FaithfulPermutationAction[P], givenBase: Seq[Int] = Seq.empty) =
    randomizedSchreierSims(chain.randomElement(_), chain.order, givenBase)(newAction)

  def completeChainFromSubgroup[S](s: S, givenBase: Seq[Int] = Seq.empty)(
    implicit action: FaithfulPermutationAction[P], subgroup: Subgroup[S, P]) =
    randomizedSchreierSims(s.randomElement(_), s.order, givenBase)

  /* Randomized BSGS Schreier-Sims using the provided procedure to generate
   * random elements and the known order of the group to terminate the algorithm.
   * 
   * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
   */
  def randomizedSchreierSims(randomElement: Random => P, order: BigInt,
    givenBase: Seq[Int] = Seq.empty)(implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val mutableChain = emptyChainWithBase(givenBase)
    while (mutableChain.start.next.order < order) {
      for ( (nodeForGenerator, generator) <- siftAndUpdateBaseFrom(mutableChain, mutableChain.start, randomElement(randomGenerator)).toOption )
        addStrongGeneratorHere(mutableChain, nodeForGenerator, generator, generator.inverse)
    }
    // TODO removeRedundantGenerators(mutableChain)
    mutableChain
  }
}
