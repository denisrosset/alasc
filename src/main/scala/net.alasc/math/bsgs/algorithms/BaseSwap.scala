package net.alasc.math
package bsgs
package algorithms

import scala.collection.mutable.{ BitSet => MutableBitSet }
import scala.collection.immutable.{ BitSet => ImmutableBitSet }

import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra.FiniteGroup
import net.alasc.syntax.subgroup._

trait BaseSwap[P] extends MutableAlgorithms[P] {
  /** Base swap.
    * 
    * @param secondNode Second base point to swap with its previous node.
    * 
    * @return the two swapped mutable nodes.
    */
  def baseSwap(mutableChain: MutableChain[P], node1: MutableNode[P], node2: MutableNode[P]): (MutableNode[P], MutableNode[P])
}

trait BaseSwapCommon[P] extends BaseSwap[P] {
  /** Swaps two adjacent nodes in the BSGS chain, and returns the first node after the swap, its tail
    * and the size goal for the orbit of the tail node.
    * 
    * The chain is not in a complete state after `prepareSwap`, so either randomized or deterministic methods
    * must be used to complete the strong generating set.
    */
  protected def prepareBaseSwap(mutableChain: MutableChain[P], node1: MutableNode[P],
    node2: MutableNode[P]): (MutableNode[P], MutableNode[P], BigInt) = {
    implicit def action = mutableChain.start.action
    val sizesProduct = BigInt(node1.orbitSize) * BigInt(node2.orbitSize)

    val (newNode1, newNode2) = mutableChain.swap(node1.prev, node1, node2, node2.next)

    (newNode1, newNode2, (sizesProduct / newNode1.orbitSize).toInt)
  }
}

trait BaseSwapDeterministic[P] extends BaseSwapCommon[P] {
  /** Deterministic base swap.
    * 
    * @inheritdoc
    * 
    * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
    * Note that their line 3 is wrong, betaT has to be used instead of betaT1.
    * 
    * See also http://www.math.uni-rostock.de/~rehn/docs/diploma-thesis-cs-rehn.pdf for an alternate
    * implementation.
    */
  def baseSwap(mutableChain: MutableChain[P], node1: MutableNode[P], node2: MutableNode[P]): (MutableNode[P], MutableNode[P]) = {
    import OrbitInstances._
    implicit def action = mutableChain.start.action
    val nodeCopy = nodeBuilder.standaloneClone(node1)
    val gammaSet = MutableBitSet.empty ++ node1.orbit
    val (newNode1, newNode2, sizeGoal2) = prepareBaseSwap(mutableChain, node1, node2)
    require(newNode1.next eq newNode2)
    gammaSet -= newNode1.beta
    gammaSet -= newNode2.beta
    while (newNode2.orbitSize < sizeGoal2) {
      val gamma = gammaSet.head
      val ipx@InversePair(x, xInv) = nodeCopy.uPair(gamma)
      val b = newNode1.beta <|+| xInv
      if (!newNode1.inOrbit(b))
        gammaSet --= ImmutableBitSet(gamma) <|+| newNode1.strongGeneratingSet
      else {
        val ipy = newNode1.uPair(b)
        val ipyx = ipy |+| ipx
        if (!newNode2.inOrbit(node2.beta <|+| ipyx)) {
          newNode2.addToOwnGenerators(ipyx)
          newNode2.updateTransversal(ipyx)
          newNode1.updateTransversal(ipyx) // TODO, remove not needed ?
          gammaSet --= ImmutableBitSet(newNode2.beta) <|+| newNode1.strongGeneratingSet
        }
        // TODO: what happens if node2.inOrbit is true ?
        // gamma will not be removed from gammaSet
        // and deterministicBaseSwap could loop forever
      }
    }
    (newNode1, newNode2)
  }
}

trait BaseSwapRandomized[P] extends BaseSwapCommon[P] with RandomizedAlgorithms {
  /** Randomized base swap.
    * 
    * @inheritdoc
    *
    * Based on algorithm 2.8 of 
    * http://www.math.uni-rostock.de/~rehn/docs/diploma-thesis-cs-rehn.pdf .
    */
  def baseSwap(mutableChain: MutableChain[P], node1: MutableNode[P], node2: MutableNode[P]): (MutableNode[P], MutableNode[P]) = {
    implicit def action = mutableChain.start.action
    val node1Copy = nodeBuilder.standaloneClone(node1)
    val node2Copy = nodeBuilder.standaloneClone(node2)
    val node2Next = node2.next
    val (newNode1, newNode2, sizeGoal2) = prepareBaseSwap(mutableChain, node1, node2)
    while (newNode2.orbitSize < sizeGoal2) {
      val g = node2Next.randomElement(randomGenerator) |+| node2Copy.randomU(randomGenerator) |+| node1Copy.randomU(randomGenerator)
      val h = g |+| newNode1.uInv(newNode1.beta <|+| g)
      val hPair = InversePair(h, h.inverse)
      if (!newNode2.inOrbit(newNode2.beta <|+| h)) {
        newNode2.addToOwnGenerators(hPair)
        newNode2.updateTransversal(hPair)
        newNode1.updateTransversal(hPair) // TODO, remove not needed ?
      }
    }
    (newNode1, newNode2)
  }
}
