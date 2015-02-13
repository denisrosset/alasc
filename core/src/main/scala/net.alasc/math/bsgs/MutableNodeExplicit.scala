package net.alasc.math
package bsgs

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random

import spire.syntax.action._
import spire.syntax.group._
import spire.syntax.eq._

import net.alasc.algebra._
import net.alasc.util._

import debox.external._

final class MutableNodeExplicit[P](
  var beta: Int,
  var transversal: SpecKeyMap[Int, InversePair[P]],
  var ownGeneratorsPairs: mutable.ArrayBuffer[InversePair[P]],
  var prev: MutableStartOrNode[P] = null,
  var next: Chain[P] = null)(implicit val action: FaithfulPermutationAction[P]) extends MutableNode[P] {

  def ownGenerators = ownGeneratorsPairs.view.map(_.g)

  def orbitSize = transversal.size
  def inOrbit(b: Int) = transversal.contains(b)
  def foreachOrbit(f: Int => Unit): Unit = transversal.foreachKey(f)
  def orbitIterator = transversal.keysIterator

  def foreachU[N](f: P => N) = transversal.foreachValue { case InversePair(u, _) => f(u) }
  def uPair(b: Int) = transversal(b)
  def u(b: Int) = transversal(b).g
  def uInv(b: Int) = transversal(b).gInv
  def iterable = new Iterable[(Int, InversePair[P])] {
    override def stringPrefix = "Iterable"
    def iterator = transversal.iterator
  }
  def randomU(rand: Random): P = u(randomOrbit(rand))

  protected def addTransversalElement(b: Int, pair: InversePair[P]): Unit = { transversal.update(b, pair) }

  protected[bsgs] def addToOwnGenerators(newGenerators: Traversable[InversePair[P]])(implicit ev: FiniteGroup[P]) = {
    ownGeneratorsPairs ++= newGenerators
  }

  protected[bsgs] def addToOwnGenerators(newGenerator: InversePair[P])(implicit ev: FiniteGroup[P]) = {
    ownGeneratorsPairs += newGenerator
  }

  import mutable.ArrayBuffer

  /** Remove redundant generators from this node generators. */
  protected[bsgs] def removeRedundantGenerators: Unit = {
    /* Tests if the orbit stays complete after removing each generator successively. 
     * The redundant generators are removed from the
     * end of the `ownGeneratorsPairs`, the non-redundant are swapped at its beginning, 
     * at the position indicated by `swapHere`.
     */
    val os = orbitSize
    var swapHere = 0
    var ogpLength = ownGeneratorsPairs.length
    while (swapHere < ogpLength) {
      val newOrbit = MutableBitSet(beta)
      var toCheck = MutableBitSet.empty
      var newAdded = MutableBitSet.empty
      @inline def swapBitsets: Unit = {
        var temp = toCheck
        toCheck = newAdded
        newAdded = temp
        newAdded.clear
      }
      {
        var j = 0
        while (j < ogpLength - 1) {
          val b = beta <|+| ownGeneratorsPairs(j).g
          newOrbit += b
          newAdded += b
          j += 1
        }
      }
      swapBitsets
      while (toCheck.nonEmpty) {
        toCheck.foreachFast { c =>
          var j = 0
          while (j < ogpLength - 1) {
            val cg = c <|+| ownGeneratorsPairs(j).g
            if (!newOrbit.contains(cg)) {
              newOrbit += cg
              newAdded += cg
            }
            j += 1
          }
          next.strongGeneratingSet.foreach { g =>
            val cg = c <|+| g
            if (!newOrbit.contains(cg)) {
              newOrbit += cg
              newAdded += cg
            }
          }
        }
        swapBitsets
      }
      if (newOrbit.size == os)
        ogpLength -= 1
      else {
        var temp = ownGeneratorsPairs(swapHere)
        ownGeneratorsPairs(swapHere) = ownGeneratorsPairs(ogpLength - 1)
        ownGeneratorsPairs(ogpLength - 1) = temp
        swapHere += 1
      }
    }
    ownGeneratorsPairs.reduceToSize(ogpLength)
  }

  protected[bsgs] def bulkAdd(beta: debox.Buffer[Int], pairs: ArrayBuffer[InversePair[P]])(implicit ev: FiniteGroup[P]) = {
    var i = 0
    val n = beta.length
    while (i < n) {
      addTransversalElement(beta(i), pairs(i))
      i += 1
    }
  }

  protected[bsgs] def updateTransversal(newGenerator: InversePair[P])(implicit ev: FiniteGroup[P]) = {
    var toCheck = MutableBitSet.empty
    val sb = new StringBuilder
    val toAddBeta = debox.Buffer.empty[Int]
    val toAddIP = ArrayBuffer.empty[InversePair[P]]
    foreachOrbit { b =>
      val newB = b <|+| newGenerator.g
      if (!inOrbit(newB)) {
        val newPair = uPair(b) |+| newGenerator
        toAddBeta += newB
        toAddIP += newPair
        toCheck += newB
      }
    }
    bulkAdd(toAddBeta, toAddIP)
    toAddBeta.clear
    toAddIP.clear
    while (!toCheck.isEmpty) {
      val newAdded = MutableBitSet.empty
      toCheck.foreachFast { b =>
        strongGeneratingSetPairs.foreach { ip =>
          val newB = b <|+| ip.g
          if (!inOrbit(newB)) {
            val newPair = uPair(b) |+| ip
            toAddBeta += newB
            toAddIP += newPair
            newAdded += newB
          }
        }
      }
      bulkAdd(toAddBeta, toAddIP)
      toAddBeta.clear
      toAddIP.clear
      toCheck = newAdded
    }
  }

  protected[bsgs] def updateTransversal(newGenerators: Traversable[InversePair[P]])(implicit ev: FiniteGroup[P]) = {
    var toCheck = MutableBitSet.empty
    val sb = new StringBuilder
    var toAddBeta = debox.Buffer.empty[Int]
    var toAddIP = ArrayBuffer.empty[InversePair[P]]
    foreachOrbit { b =>
      newGenerators.foreach { newGenerator =>
        val newB = b <|+| newGenerator.g
        if (!inOrbit(newB)) {
          val newPair = uPair(b) |+| newGenerator
          toAddBeta += newB
          toAddIP += newPair
          toCheck += newB
        }
      }
    }
    bulkAdd(toAddBeta, toAddIP)
    toAddBeta.clear
    toAddIP.clear
    while (!toCheck.isEmpty) {
      val newAdded = MutableBitSet.empty
      toCheck.foreachFast { b =>
        strongGeneratingSetPairs.foreach { ip =>
          val InversePair(g, gInv) = ip
          val newB = b <|+| g
          if (!inOrbit(newB)) {
            val newPair = uPair(b) |+| ip
            toAddBeta += newB
            toAddIP += newPair
            newAdded += newB
          }
        }
      }
      bulkAdd(toAddBeta, toAddIP)
      toAddBeta.clear
      toAddIP.clear
      toCheck = newAdded
    }
  }

  protected[bsgs] def conjugate(ip: InversePair[P])(implicit ev: FiniteGroup[P]) = {
    beta = beta <|+| ip.g
    val newTransversal = SpecKeyMap.empty[Int, InversePair[P]]
    transversal.foreachKey { k =>
      val newG: P = ip.gInv |+| transversal(k).g |+| ip.g
      newTransversal.update(k <|+| ip.g, InversePair(newG, newG.inverse))
    }
    transversal = newTransversal
    ownGeneratorsPairs.transform {
      case InversePair(g, gInv) =>
        val newG = ip.gInv |+| g |+| ip.g
        InversePair(newG, newG.inverse)
    }
  }
}

class MutableNodeExplicitBuilder[P] extends NodeBuilder[P] {
  def standaloneClone(node: Node[P])(implicit algebra: FiniteGroup[P]) = node match {
    case mne: MutableNodeExplicit[P] =>
      new MutableNodeExplicit(mne.beta, mne.transversal.copy, mne.ownGeneratorsPairs.clone)(mne.action)
    case _ =>
      val newTransversal = SpecKeyMap.fromIterable[Int, InversePair[P]](node.iterable)
      val newOwnGeneratorsPairs = mutable.ArrayBuffer.empty[InversePair[P]] ++= node.ownGeneratorsPairs
      new MutableNodeExplicit(node.beta, newTransversal, newOwnGeneratorsPairs)(node.action)
  }
  def standalone(beta: Int)(implicit action: FaithfulPermutationAction[P], algebra: FiniteGroup[P]) =
    new MutableNodeExplicit(beta,
      SpecKeyMap(beta -> InversePair(algebra.id, algebra.id)),
      mutable.ArrayBuffer.empty[InversePair[P]])
}
