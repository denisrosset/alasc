package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.action._
import spire.syntax.order._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.perms.MutableOrbit
import metal.syntax._

import net.alasc.perms.orbits

abstract class BaseSwap {

  /** Swaps `node1` with its next node `node2`.
    * 
    * @return the two swapped mutable nodes.
    */
  def baseSwap[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (mutableChain: MutableChain[G, F], node1: MutableNode[G, F], node2: MutableNode[G, F]): MutableNodeAndNext[G, F]


}

final class BaseSwapDeterministic extends BaseSwap {

  /** Deterministic base swap.
    * 
    * @inheritdoc
    * 
    * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
    * Note that their line 3 is wrong, betaT has to be used instead of betaT1.
    * 
    * See also http://www.math.uni-rostock.de/~rehn/docs/diploma-thesis-cs-rehn.pdf 
    * for an alternate implementation.
    */
  def baseSwap[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (mutableChain: MutableChain[G, F], node1: MutableNode[G, F], node2: MutableNode[G, F]): MutableNodeAndNext[G, F] = {
    implicit def action: F = mutableChain.start.action
    val gammaSet = metal.mutable.FixedBitSet.fromIterable(node1.orbit)
    var gamma = -1
    val mutableOrbit = MutableOrbit.empty
    val (newNode1, newNode2, sizeGoal2) = mutableChain.prepareSwap(node1.prev, node1, node2, node2.next)
    require(newNode1.next eq newNode2)
    gammaSet -= newNode1.beta
    gammaSet -= newNode2.beta
    while (newNode2.orbitSize < sizeGoal2) {
      gamma = gammaSet.findOrNextAfter(gamma + 1).get.key // assume that the set is always nonempty
      val x = node1.u(gamma)
      val xInv = node1.uInv(gamma)
//      assert((newNode2.beta <|+| x) == gamma)
      val b = newNode1.beta <|+| xInv
      if (!node2.inOrbit(b)) {
        //        val bm = Orbits.orbitBitMask(gamma, newNode2.strongGeneratingSet, Opt(mutableOrbit))
        //        gammaSet &~= scala.collection.mutable.BitSet.fromBitMaskNoCopy(bm)
        mutableOrbit.addNew(gamma)
        orbits.Points.iterateOrbit(mutableOrbit, gamma, newNode2.strongGeneratingSet)
        MutableOrbit.&~=(gammaSet, mutableOrbit)
        mutableOrbit.clear()
      } else {
        val y = node2.u(b)
        val yInv = node2.uInv(b)
        val yx = y |+| x
        val yxInv = xInv |+| yInv
        if (!newNode2.inOrbit(node2.beta <|+| yx)) {
          newNode2.addToOwnGenerators(yx, yxInv)
          newNode2.updateTransversal(yx, yxInv)
          newNode2.foreachOrbit( gammaSet -= _ )
        }
      }
    }
    MutableNodeAndNext(newNode1, newNode2)
  }

}

final class BaseSwapRandom(val random: Random) extends BaseSwap {

    /** Randomized base swap.
    * 
    * @inheritdoc
    *
    * Based on algorithm 2.8 of 
    * http://www.math.uni-rostock.de/~rehn/docs/diploma-thesis-cs-rehn.pdf .
    */
  def baseSwap[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (mutableChain: MutableChain[G, F], node1: MutableNode[G, F], node2: MutableNode[G, F]): MutableNodeAndNext[G, F] = {
    implicit def action: PermutationAction[G] = mutableChain.start.action
    val node2next = node2.next
    val (newNode1, newNode2, sizeGoal2) = mutableChain.prepareSwap(node1.prev, node1, node2, node2.next)
    while (newNode2.orbitSize < sizeGoal2) {
      val g = node2next.randomElement(random) |+| node2.randomU(random) |+| node1.randomU(random)
      val h = g |+| newNode1.uInv(newNode1.beta <|+| g)
      val hInv = h.inverse
      if (!newNode2.inOrbit(newNode2.beta <|+| h)) {
        newNode2.addToOwnGenerators(h, hInv)
        newNode2.updateTransversal(h, hInv)
      }
    }
    MutableNodeAndNext(newNode1, newNode2)
  }

}

object BaseSwap {

  val deterministic: BaseSwap = new BaseSwapDeterministic

  def randomized(implicit random: Random): BaseSwap = new BaseSwapRandom(random)

}
