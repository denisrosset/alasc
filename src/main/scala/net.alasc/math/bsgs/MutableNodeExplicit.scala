package net.alasc.math
package bsgs

import scala.annotation.tailrec
import scala.collection.mutable.{BitSet => MutableBitSet, LongMap => MutableLongMap, UnrolledBuffer}

import scala.util.Random

import spire.syntax.groupAction._
import spire.syntax.group._
import spire.syntax.eq._

import net.alasc.algebra._

final class MutableNodeExplicit[P](
  var beta: Int,
  var transversal: MutableLongMap[InversePair[P]],
  var ownGeneratorsPairs: UnrolledBuffer[InversePair[P]],
  var prev: MutableStartOrNode[P] = null,
  var next: Chain[P] = null)(implicit val action: FaithfulPermutationAction[P]) extends MutableNode[P] {

  def ownGenerators = ownGeneratorsPairs.view.map(_.g)

  def orbitSize = transversal.size
  def inOrbit(b: Int) = transversal.isDefinedAt(b)
  def foreachOrbit[U](f: Int => U): Unit =
    transversal.foreachKey( k => f(k.toInt) )
  def orbit = new Iterable[Int] {
    override def stringPrefix = "Iterable"
    override def foreach[U](f: Int => U) = foreachOrbit(f)
    def iterator = transversal.keysIterator.map(_.toInt)
  }
  def randomOrbit(rand: Random): Int = orbit.drop(rand.nextInt(orbitSize)).head

  def foreachU[N](f: P => N) = transversal.foreachValue { case InversePair(u, _) => f(u) }
  def uPair(b: Int) = transversal(b)
  def u(b: Int) = transversal(b).g
  def uInv(b: Int) = transversal(b).gInv
  def iterable = new Iterable[(Int, InversePair[P])] {
    override def stringPrefix = "Iterable"
    override def foreach[U](f: ((Int, InversePair[P])) => U) = transversal.foreach { case (k, v) => f((k.toInt, v)) }
    def iterator = transversal.iterator.map { case (k, v) => (k.toInt, v) }
  }
  def randomU(rand: Random): P = u(randomOrbit(rand))

  protected def addTransversalElement(b: Int, pair: InversePair[P]): Unit = { transversal.update(b, pair) }

  protected[bsgs] def addToOwnGenerators(newGenerators: Traversable[InversePair[P]])(implicit ev: FiniteGroup[P]) = {
    ownGeneratorsPairs ++= newGenerators
  }

  protected[bsgs] def addToOwnGenerators(newGenerator: InversePair[P])(implicit ev: FiniteGroup[P]) = {
    ownGeneratorsPairs += newGenerator
  }

  import scala.collection.mutable.ArrayBuffer

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
      toCheck.foreach { b =>
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
      toCheck.foreach { b =>
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
    val newTransversal = MutableLongMap.empty[InversePair[P]]
    transversal.foreachKey { k =>
      newTransversal.update(k.toInt <|+| ip, ip.inverse |+| transversal(k) |+| ip)
    }
    transversal = newTransversal
    ownGeneratorsPairs.transform(g => ip.inverse |+| g |+| ip)
  }
}

class MutableNodeExplicitBuilder[P] extends NodeBuilder[P] {
  def standaloneClone(node: Node[P])(implicit algebra: FiniteGroup[P]) = node match {
    case mne: MutableNodeExplicit[P] =>
      new MutableNodeExplicit(mne.beta, mne.transversal.clone, mne.ownGeneratorsPairs.clone)(mne.action)
    case _ =>
      val newTransversal = MutableLongMap.empty[InversePair[P]] ++= node.iterable.map { case (k, v) => (k.toLong, v) }
      val newOwnGeneratorsPairs = UnrolledBuffer.empty[InversePair[P]] ++= node.ownGeneratorsPairs
      new MutableNodeExplicit(node.beta, newTransversal, newOwnGeneratorsPairs)(node.action)
  }
  def standalone(beta: Int)(implicit action: FaithfulPermutationAction[P], algebra: FiniteGroup[P]) =
    new MutableNodeExplicit(beta,
      MutableLongMap(beta.toLong -> InversePair(algebra.id, algebra.id)),
      UnrolledBuffer.empty[InversePair[P]])
}
