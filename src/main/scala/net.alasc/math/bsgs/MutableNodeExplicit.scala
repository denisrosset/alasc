package net.alasc.math
package bsgs

import scala.annotation.tailrec
import scala.collection.mutable.{BitSet => MutableBitSet, LongMap => MutableLongMap, UnrolledBuffer}

import scala.util.Random

import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra._

final class MutableNodeExplicit[P](
  var beta: Int,
  var transversal: MutableLongMap[InversePair[P]],
  var ownGeneratorsPairs: UnrolledBuffer[InversePair[P]],
  var prev: MutableStartOrNode[P] = null,
  var next: Chain[P] = null)(implicit val action: PermutationAction[P]) extends MutableNode[P] {

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

  protected[bsgs] def addToOwnGenerators(g: InversePair[P])(implicit ev: FiniteGroup[P]) = { ownGeneratorsPairs += g }

  protected[bsgs] def updateTransversal(newGenerator: InversePair[P])(implicit ev: FiniteGroup[P]) = {
    var toCheck = MutableBitSet.empty
    foreachOrbit { b =>
      val newB = b <|+| newGenerator.g
      if (!inOrbit(newB)) {
        val newPair = uPair(b) |+| newGenerator
        addTransversalElement(newB, newPair)
        toCheck += newB
      }
    }
    while (!toCheck.isEmpty) {
      val newAdded = MutableBitSet.empty
      for (ip@InversePair(g, gInv) <- strongGeneratingSetPairs; b <- toCheck) {
        val newB = b <|+| g
        if (!inOrbit(newB)) {
          val newPair = uPair(b) |+| ip
          addTransversalElement(newB, newPair)
          newAdded += newB
        }
      }
      toCheck = newAdded
    }
  }

  protected[bsgs] def conjugateThisNode(ip: InversePair[P])(implicit ev: FiniteGroup[P]): Unit = {
    val newBeta = beta <|+| ip
    val newTransversal = MutableLongMap.empty[InversePair[P]]
    for ( (b, pair) <- transversal )
      newTransversal.update(b.toInt <|+| ip, ip.inverse |+| pair |+| ip)
    beta = newBeta
    transversal = newTransversal
  }

  protected[bsgs] def changeBasePoint(newBeta: Int, pred: P => Boolean)(implicit ev: FiniteGroup[P]): Iterable[InversePair[P]] = {
    val (keep, removedPairs) = ownGeneratorsPairs.partition(ip => pred(ip.g))
    ownGeneratorsPairs = keep
    beta = newBeta
    transversal.clear
    transversal.update(newBeta, ev.id)
    strongGeneratingSetPairs.foreach { ip => updateTransversal(ip) }
    removedPairs
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
  def standalone(beta: Int)(implicit action: PermutationAction[P], algebra: FiniteGroup[P]) =
    new MutableNodeExplicit(beta,
      MutableLongMap(beta.toLong -> InversePair(algebra.id, algebra.id)),
      UnrolledBuffer.empty[InversePair[P]])
}
