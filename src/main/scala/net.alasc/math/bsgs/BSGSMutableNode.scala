package net.alasc.math
package bsgs

import scala.annotation.tailrec
import scala.collection.mutable.{BitSet => MutableBitSet}
import scala.util.Random
import spire.syntax.groupAction._
import spire.syntax.group._
import net.alasc.algebra._

/*TODO protected[alasc]*/ trait BSGSMutableNode[P] extends BSGSNode[P] {
  var beta: Int

  var tail: BSGS[P]

  /** Reference to the previous node in a mutable BSGS chain.
    *
    * Has special values:
    * 
    * - `null` when this mutable BSGS node has been exported as immutable,
    * - a self `this` reference is used to terminate the chain at the beginning.
    */
  var prev: BSGSMutableNode[P]

  /** Adds a new generator `g` (given as an `InversePair`) to this node `ownGenerators`.
    */
  def addToOwnGenerators(g: InversePair[P]): Unit

  /** Updates this node transversal by the addition of a new generator `g`,
    * provided as an `InversePair`.
    * 
    * @note `g` must be already part of the strong generating set, i.e.
    *       have been added to this node or a children node `ownGenerators`
    *       by using addToOwnGenerators once.
    */
  def updateTransversal(g: InversePair[P]): Unit

  /** Adds the new strong generator `g`, provided as an `InversePair`, at this node level, and update the transversal. */
  def addStrongGeneratorHere(g: InversePair[P]): Unit = {
    addToOwnGenerators(g)
    @tailrec def rec(node: BSGSMutableNode[P]): Unit = {
      updateTransversal(g)
      if (node.prev ne node)
        rec(node.prev)
    }
    rec(this)
  }

  def conjugateThisNode(ip: InversePair[P]): Unit

  def clearTransversalAndKeepOwnGenerators: Unit
  def clearTransversalAndReplaceOwnGenerators(newOG: Iterable[InversePair[P]]): Unit
}

protected[alasc] trait BSGSMutableNodeBuilder {
  /** Creates a mutable copy of a BSGS node.
    * 
    * @param bsgs   Node to clone
    * @param prev   Reference to the previous node in the chain, or `None` if the mutable copy
    *               is at the beginning of the chain (in that case, the new mutable node `prev`
    *               will point to itself).
    * 
    * @note         The created node `tail` previous reference `prev` is not updated.
    *               The tail of `prev` is not updated.
    */
  def apply[P](bsgs: BSGSNode[P], prev: Option[BSGSMutableNode[P]]): BSGSMutableNode[P]
  /** Creates a new mutable BSGS node with an empty transversal.
    * 
    * @param beta  New node base point.
    * @param prev   Reference to the previous node in the chain, or `None` if the mutable copy
    *               is at the beginning of the chain (in that case, the new mutable node `prev`
    *               will point to itself).
    * @param tail  Tail of the new node.
    * 
    * @note         The created node `tail` previous reference `prev` is not updated.
    *               The tail of `prev` is not updated.
    */
  def apply[P](beta: Int, prev: Option[BSGSMutableNode[P]], tail: BSGS[P])(implicit algebra: Permutation[P]): BSGSMutableNode[P]
}

object BSGSMutableNodeBuilder {
  implicit def instance: BSGSMutableNodeBuilder = BSGSMutableNodeExplicit
}

import scala.collection.mutable.{LongMap => MutableLongMap, UnrolledBuffer}

object BSGSMutableNodeExplicit extends BSGSMutableNodeBuilder {
  def apply[P](bsgs: BSGSNode[P], prev: Option[BSGSMutableNode[P]]) = {
    val (newTransversal, newOwnGeneratorsPairs) = bsgs match {
      case bmne: BSGSMutableNodeExplicit[P] => (bmne.transversal.clone, bmne.ownGeneratorsPairs.clone)
      case _ => (MutableLongMap.empty[InversePair[P]] ++= bsgs.iterable.map { case (k, v) => (k.toLong, v) },
        UnrolledBuffer.empty[InversePair[P]] ++ bsgs.ownGeneratorsPairs)
    }
    val newNode = new BSGSMutableNodeExplicit[P](bsgs.beta, newTransversal, newOwnGeneratorsPairs, null, bsgs.tail)(bsgs.algebra)
    newNode.prev = prev.getOrElse(newNode)
    newNode
  }
  def apply[P](beta: Int, prev: Option[BSGSMutableNode[P]], tail: BSGS[P])(implicit algebra: Permutation[P]) = {
    val newNode = new BSGSMutableNodeExplicit(beta,
      MutableLongMap(beta.toLong -> InversePair(algebra.id, algebra.id)),
      UnrolledBuffer.empty[InversePair[P]],
      null,
      tail)
    newNode.prev = prev.getOrElse(newNode)
    newNode
  }
}

protected[alasc] final class BSGSMutableNodeExplicit[P](
  var beta: Int,
  var transversal: MutableLongMap[InversePair[P]],
  val ownGeneratorsPairs: UnrolledBuffer[InversePair[P]],
  var prev: BSGSMutableNode[P],
  var tail: BSGS[P])(implicit val algebra: Permutation[P]) extends BSGSMutableNode[P] {

  def isImmutable = prev eq null
  def ownGenerators = ownGeneratorsPairs.map(_.g)

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

  def addToOwnGenerators(g: InversePair[P]) = { ownGeneratorsPairs += g }

  def addTransversalElement(b: Int, pair: InversePair[P]): Unit = { transversal.update(b, pair) }

  def updateTransversal(newGenerator: InversePair[P]) = {
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

  def conjugatedBy(ip: InversePair[P]): BSGS[P] = BSGSBuilder(this).conjugate(ip).toBSGS

  def conjugateThisNode(ip: InversePair[P]): Unit = {
    val newBeta = beta <|+| ip
    val newTransversal = MutableLongMap.empty[InversePair[P]]
    for ( (b, pair) <- transversal )
      newTransversal.update(b.toInt <|+| ip, ip.inverse |+| pair |+| ip)
    beta = newBeta
    transversal = newTransversal
  }

  def clearTransversalAndKeepOwnGenerators = {
    transversal.clear
  }

  def clearTransversalAndReplaceOwnGenerators(newOG: Iterable[InversePair[P]]) = {
    transversal.clear
    ownGeneratorsPairs.clear
    ownGeneratorsPairs ++= newOG
  }
}
