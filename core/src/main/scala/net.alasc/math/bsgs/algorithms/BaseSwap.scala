package net.alasc.math
package bsgs
package algorithms

import scala.collection.{immutable, mutable}

import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra.InversePair
import net.alasc.syntax.subgroup._

trait BaseSwap[P] extends MutableAlgorithms[P] {
  /** Base swap.
    * 
    * @param secondNode Second base point to swap with its previous node.
    * 
    * @return the two swapped mutable nodes.
    */
  def baseSwap(mutableChain: MutableChain[P], node1: MutableNode[P], node2: MutableNode[P]): MutableNodeAndNext[P]
}

trait BaseSwapCommon[P] extends BaseSwap[P] {
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
  def baseSwap(mutableChain: MutableChain[P], node1: MutableNode[P], node2: MutableNode[P]): MutableNodeAndNext[P] = {
    import net.alasc.math.OrbitInstances._
    implicit def action = mutableChain.start.action
    val gammaSet = mutable.BitSet.empty ++ node1.orbit
    val (newNode1, newNode2, sizeGoal2) = mutableChain.prepareSwap(node1.prev, node1, node2, node2.next)
    require(newNode1.next eq newNode2)
    gammaSet -= newNode1.beta
    gammaSet -= newNode2.beta
    while (newNode2.orbitSize < sizeGoal2) {
      val gamma = gammaSet.head
      val ipx@InversePair(x, xInv) = node1.uPair(gamma)
      assert((newNode2.beta <|+| x) == gamma)
      val b = newNode1.beta <|+| xInv
      if (!node2.inOrbit(b))
        gammaSet --= immutable.BitSet(gamma) <|+| newNode2.strongGeneratingSet
      else {
        val ipy = node2.uPair(b)
        val ipyx = ipy |+| ipx
        if (!newNode2.inOrbit(node2.beta <|+| ipyx.g)) {
          newNode2.addToOwnGenerators(ipyx.g, ipyx.gInv)
          newNode2.updateTransversal(ipyx.g, ipyx.gInv)
          gammaSet --= newNode2.orbitSet
        }
      }
    }
    MutableNodeAndNext(newNode1, newNode2)
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
  def baseSwap(mutableChain: MutableChain[P], node1: MutableNode[P], node2: MutableNode[P]): MutableNodeAndNext[P] = {
    implicit def action = mutableChain.start.action
    val node2next = node2.next
    val (newNode1, newNode2, sizeGoal2) = mutableChain.prepareSwap(node1.prev, node1, node2, node2.next)
    while (newNode2.orbitSize < sizeGoal2) {
      val g = node2next.randomElement(randomGenerator) |+| node2.randomU(randomGenerator) |+| node1.randomU(randomGenerator)
      val h = g |+| newNode1.uInv(newNode1.beta <|+| g)
      val hPair = InversePair(h, h.inverse)
      if (!newNode2.inOrbit(newNode2.beta <|+| h)) {
        newNode2.addToOwnGenerators(hPair.g, hPair.gInv)
        newNode2.updateTransversal(hPair.g, hPair.gInv)
      }
    }
    MutableNodeAndNext(newNode1, newNode2)
  }
}
