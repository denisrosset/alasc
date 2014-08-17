package net.alasc.math
package bsgs
package algorithms

import scala.collection.mutable.{ BitSet => MutableBitSet }
import scala.collection.immutable.{ BitSet => ImmutableBitSet }

import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra.FiniteGroup

trait BaseSwap[P] extends MutableAlgorithms[P] {
  /** Base swap.
    * 
    * @param firstNode First base point to swap with its next node.
    * 
    * @return the two swapped mutable nodes.
    */
  def baseSwap(mutableChain: MutableChain[P], firstNode: MutableNode[P]): (MutableNode[P], MutableNode[P])
}

trait BaseSwapCommon[P] extends BaseSwap[P] {
  /** Swaps two adjacent nodes in the BSGS chain, and returns the first node after the swap, its tail
    * and the size goal for the orbit of the tail node.
    * 
    * The chain is not in a complete state after `prepareSwap`, so either randomized or deterministic methods
    * must be used to complete the strong generating set.
    */
  protected def prepareBaseSwap(mutableChain: MutableChain[P],
    node2: MutableNode[P]): (MutableNode[P], MutableNode[P], BigInt) = {
    implicit def action = mutableChain.start.action
    val node1 = IsMutableNode.unapply(node2.prev).getOrElse(sys.error("Previous element must be mutable node."))
    val sizesProduct = BigInt(node1.orbitSize) * BigInt(node2.orbitSize)

    val newBeta1 = node2.beta
    val newBeta2 = node1.beta

    node1.changeBasePoint(newBeta1, g => true) // keep all generators for node1
    val removedOwnGenerators = node2.changeBasePoint(newBeta2, g => (newBeta1 <|+| g) == newBeta1)
    removedOwnGenerators.foreach { g =>
      node1.addToOwnGenerators(g)
      node1.updateTransversal(g)
    }
    (node1, node2, (sizesProduct / node1.orbitSize).toInt)
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
  def baseSwap(mutableChain: MutableChain[P],
    firstNode: MutableNode[P]): (MutableNode[P], MutableNode[P]) = {
    import OrbitInstances._
    implicit def action = mutableChain.start.action
    val nodeCopy = nodeBuilder.standaloneClone(firstNode)
    val gammaSet = MutableBitSet.empty ++ firstNode.orbit
    val (node1, node2, sizeGoal2) = prepareBaseSwap(mutableChain, firstNode)
    gammaSet -= node1.beta
    gammaSet -= node2.beta
    while (node2.orbitSize < sizeGoal2) {
      val gamma = gammaSet.head
      val ipx@InversePair(x, xInv) = nodeCopy.uPair(gamma)
      val b = node2.beta <|+| xInv
      if (!node2.inOrbit(b))
        gammaSet --= ImmutableBitSet(gamma) <|+| node1.strongGeneratingSet
      else {
        val ipy = node2.uPair(b)
        val ipyx = ipy |+| ipx
        if (!node2.inOrbit(node2.beta <|+| ipyx)) {
          node2.addToOwnGenerators(ipyx)
          node2.updateTransversal(ipyx)
          node1.updateTransversal(ipyx)
          gammaSet --= ImmutableBitSet(node2.beta) <|+| node1.strongGeneratingSet
        }
        // TODO: what happens if node2.inOrbit is true ?
        // gamma will not be removed from gammaSet
        // and deterministicBaseSwap could loop forever
      }
    }
    (node1, node2)
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
  def baseSwap(mutableChain: MutableChain[P],
    firstNode: MutableNode[P]): (MutableNode[P], MutableNode[P]) = {
    implicit def action = mutableChain.start.action
    val nodeCopy = nodeBuilder.standaloneClone(firstNode)
    val (node1, node2, sizeGoal2) = prepareBaseSwap(mutableChain, firstNode)
    while (node2.orbitSize < sizeGoal2) {
      val g = nodeCopy.randomU(randomGenerator) // TODO: |+| with random element of node2.next ?
      val h = g |+| node1.uInv(node1.beta <|+| g)
      val hPair = InversePair(h, h.inverse)
      if (!node2.inOrbit(node2.beta <|+| h)) {
        node2.addToOwnGenerators(hPair)
        node2.updateTransversal(hPair)
        node1.updateTransversal(hPair)
      }
    }
    (node1, node2)
  }
}
