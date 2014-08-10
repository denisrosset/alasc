package net.alasc.math
package bsgs

import spire.syntax.groupAction._
import spire.syntax.group._
import spire.syntax.eq._
import scala.annotation.tailrec
import net.alasc.algebra._
import net.alasc.syntax.permutation._
import net.alasc.syntax.subgroup._

object BSGSBuilder {
  /** Constructs a BSGS builder from an immutable BSGS chain. */
  def apply[P](immutableBSGS: BSGS[P]): BSGSBuilder[P] = {
    assert(immutableBSGS.isImmutable)
    val builder = new BSGSBuilder()(immutableBSGS.algebra)
    builder.start = immutableBSGS
    builder
  }
}

/** Builder for a BSGS chain.
  * 
  * A BSGSBuilder can be in three different states:
  * 
  * - fully immutable: no node is mutable, mutable operations will copy nodes as needed,
  * - partly mutable: the beginning of the chain is mutable, while the end is immutable,
  * - fully mutable: the whole chain is mutable.
  * 
  * Mutable nodes are identified because their `prev` reference is initialized so that the
  * mutable part of the BSGS chain is a double-linked list.
  * 
  * If a node is mutable (= is part of a BSGSBuilder, and has not been exported), then its `prev` is not null, and
  * either points to this node's parent it has one, or to this node itself if this node is the first in the chain.
  *
  * Moreover, if a node is mutable, all its parents must be mutable.
  * 
  * If the node is immutable, then `prev` is `null`.
  *
  * When the chain is fully/partly mutable, `last0` points to the last mutable node.
  * 
  * From those assumptions the state of the builder is valid for:
  * 
  * - when fully immutable: `start` is either a terminal, or `start.prev` is `null`, and `last0` is null,
  * - when partly/fully mutable: `start` is a node, `start.prev` points to `start`, and `last0` points to the last mutable node,
  * - when fully mutable: additionally, `last0.tail` is a terminal node.
  * 
  * `BSGSBuilder` is not thread-safe.
  */
final class BSGSBuilder[P](implicit val algebra: Permutation[P]) {
  /** Start of the BSGS chain. */
  private var start: BSGS[P] = new BSGSTerm[P]

  def chain: BSGS[P] = start

  /** Last mutable node in the chain. */
  private var lastMutable: BSGSMutableNode[P] = null

  def isEmpty = start.isInstanceOf[BSGSTerm[P]]

  def isFullyImmutable: Boolean = lastMutable eq null

  def isPartlyMutable: Boolean = (lastMutable ne null) && lastMutable.tail.isInstanceOf[BSGSNode[P]]

  def isFullyMutable: Boolean = (lastMutable ne null) && lastMutable.tail.isInstanceOf[BSGSTerm[P]]

  def makeImmutable: Unit = if (lastMutable ne null) {
    @tailrec def removePrev(node: BSGSMutableNode[P]): Unit = {
      val prevSave = node.prev
      node.prev = null
      if (node ne prevSave) removePrev(prevSave)
    }
    lastMutable = null
  }

  def toBSGS: BSGS[P] = {
    makeImmutable
    start
  }

  def foreachMutable[A](f: BSGSMutableNode[P] => A): Unit = {
    @tailrec def rec(node: BSGSMutableNode[P]): Unit = {
      f(node)
      if (node.prev ne node)
        rec(node.prev)
    }
    if (lastMutable ne null)
      rec(lastMutable)
  }

  def conjugate(ip: InversePair[P]): this.type = {
    if (!isEmpty) {
      makeFullyMutable(sys.error("Cannot be empty"))
      foreachMutable { _.conjugateThisNode(ip) }
    }
    this
  }

  @tailrec def findLastNode(node: BSGSNode[P]): BSGSNode[P] = node.tail match {
    case _: BSGSTerm[P] => node
    case tailNode: BSGSNode[P] => findLastNode(tailNode)
  }

  def makeMutable(upTo: BSGSNode[P])(implicit builder: BSGSMutableNodeBuilder): BSGSMutableNode[P] = {
    @tailrec def createMutable(at: BSGSNode[P], prev: BSGSMutableNode[P]): BSGSMutableNode[P] =
      at match {
        case _ if at.isImmutable =>
          val newNode = builder(at, Some(prev))
          prev.tail = newNode
          lastMutable = newNode
          if (at ne upTo) // current node has children, including upTo
            createMutable(newNode.tail.asInstanceOf[BSGSNode[P]], newNode)
          else
            newNode
        case bm: BSGSMutableNode[P] =>
          if (bm ne upTo)
            createMutable(bm.tail.asInstanceOf[BSGSNode[P]], bm)
          else
            bm
        case _ => sys.error("A node should either be immutable or an instance of BSGSMutableNode.")
      }
    if (upTo.isImmutable) {
      val upToIsStart = start eq upTo
      if (lastMutable eq null) {
        assert(start.isInstanceOf[BSGSNode[P]]) // if upTo is a node, then start is a node
        val startNode = start.asInstanceOf[BSGSNode[P]]
        val newStartNode = builder(startNode, None)
        start = newStartNode
        lastMutable = newStartNode
      }
      if (!upToIsStart)  // if start ne upTo -> lastMutable has children including upTo
        createMutable(lastMutable.tail.asInstanceOf[BSGSNode[P]], lastMutable)
      else
        start.asInstanceOf[BSGSMutableNode[P]]
    }
    else upTo.asInstanceOf[BSGSMutableNode[P]]
  }

  def makeFullyMutable(beta: Int)(implicit builder: BSGSMutableNodeBuilder): Unit = start match {
    case _: BSGSTerm[P] =>
      val newStart = builder(beta, None, start)
      start = newStart
      lastMutable = newStart
    case node: BSGSNode[P] => if (!isFullyMutable) makeMutable(findLastNode(node))
  }

  /** Appends a new base point to the BSGS chain. */
  def append(beta: Int)(implicit builder: BSGSMutableNodeBuilder): BSGSMutableNode[P] = {
    if (isEmpty)
      mutableStartNode(beta)
    else {
      val newNode = builder(beta, Some(lastMutable), lastMutable.tail)
      lastMutable.tail = newNode
      lastMutable = newNode
      newNode
    }
  }

  /** Prepends a base point to the BSGS chain.
    * 
    * @param beta  New base point to prepend, must be fixed by all strong generators
    *              for the chain to stay valid after insertion.
    */
  def preprend(beta: Int)(implicit builder: BSGSMutableNodeBuilder) =
    if (isEmpty)
      makeFullyMutable(beta)
    else {
      val newNode = builder(beta, None, start)
      if (!start.isImmutable)
        start.asInstanceOf[BSGSMutableNode[P]].prev = newNode
      start = newNode
      if (lastMutable eq null)
        lastMutable = newNode
    }

  /** Prepares the chain such that it starts with mutable base elements such that no generator
    * stabilizes all of them.
    * 
    * @param generators  Non-empty sequence of generators. Must not contain the identity.
    * @return the node at the end of the mutable base
    */
  def mutableBaseForGenerators(generators: Seq[P])(implicit builder: BSGSMutableNodeBuilder): BSGSMutableNode[P] = {
    @tailrec def rec(mutableNode: BSGSMutableNode[P], remaining: Seq[P]): BSGSMutableNode[P] = {
      val nextRemaining = remaining.filterNot(g => (mutableNode.beta <|+| g) != mutableNode.beta)
      if (nextRemaining.isEmpty)
        mutableNode
      else mutableNode.tail match {
        case _: BSGSTerm[P] =>
          val nextBeta = nextRemaining.head.supportMin
          assert(nextBeta != -1)
          rec(append(nextBeta), nextRemaining)
        case tailNode: BSGSNode[P] =>
          rec(makeMutable(tailNode), nextRemaining)
      }
    }
    rec(mutableStartNode(generators.head.supportMin), generators)
  }

  /** Adds the given generators to the BSGS chain, adding base elements
    * if necessary, and updates the transversals.
    * 
    * @param generators  Sequence of generators. Must not contain the identity.
    */
  def addGenerators(generators: Seq[P])(implicit builder: BSGSMutableNodeBuilder): Unit = {
    if (generators.isEmpty) return

    @tailrec def rec(mutableNode: BSGSMutableNode[P], remaining: Seq[P]): Unit = {
      val (generatorsThere, newRemaining) = remaining.partition(g => (mutableNode.beta <|+| g) != mutableNode.beta)
      generatorsThere.foreach( mutableNode.addStrongGeneratorHere(_) )
      if (mutableNode.prev ne mutableNode)
        rec(mutableNode.prev, newRemaining)
    }
    rec(mutableBaseForGenerators(generators), generators)
  }

  def mutableStartNode(beta: => Int)(implicit builder: BSGSMutableNodeBuilder): BSGSMutableNode[P] = start match {
    case _: BSGSTerm[P] =>
      val newNode = builder(beta, None, start)
      start = newNode
      lastMutable = newNode
      newNode
    case node: BSGSNode[P] => makeMutable(node)
  }

  def startNode(beta: => Int)(implicit builder: BSGSMutableNodeBuilder): BSGSNode[P] = start match {
    case _: BSGSTerm[P] => mutableStartNode(beta)
    case node: BSGSNode[P] => node
  }

  @tailrec def siftAndUpdateBaseFrom(node: BSGSNode[P], p: P)(implicit builder: BSGSMutableNodeBuilder): Option[(BSGSMutableNode[P], P)] = {
    val b = node.beta <|+| p
    if (!node.inOrbit(b)) return Some(makeMutable(node) -> p)
    val h = p |+| node.uInv(b)
    assert(node.beta <|+| h == node.beta) // TODO remove
    node.tail match {
      case _: BSGSTerm[P] if h.isId => None
      case _: BSGSTerm[P] =>
        val newBasePoint = h.supportMin
        assert(newBasePoint != -1) // there is a support for h =!= identity
        Some(append(newBasePoint) -> h)
      case tailNode: BSGSNode[P] => siftAndUpdateBaseFrom(tailNode, h)
    }
  }

  /** Sifts the element `p` through the BSGS chain, and returns either:
    * 
    * - `None` if `p` can be sifted completely,
    * - `Some(pair)` where `pair` describes the node and the
    *   (incompletely) sifted element that should be inserted up to it.
    * 
    * Will insert new base points if necessary.
    * 
    * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
    */
  def siftAndUpdateBase(p: P)(implicit builder: BSGSMutableNodeBuilder): Option[(BSGSMutableNode[P], P)] =
    siftAndUpdateBaseFrom(startNode(p.supportMin.max(0)), p)


  /** Finds if a new strong generator can be found at the given `node`, assuming that
    * the chain starting at `node.tail` is complete.
    * 
    * If a new strong generator is found, returns some pair containing
    * the node and the strong generator to insert there.
    */
  def findNewStrongGeneratorAt(node: BSGSNode[P])(implicit builder: BSGSMutableNodeBuilder): Option[(BSGSMutableNode[P], P)] = {
    node.foreachOrbit { b =>
      val ub = node.u(b)
      for (x <- node.strongGeneratingSet) {
        val i = b <|+| x
        val ubx = ub |+| x
        if (ubx =!= node.u(i)) {
          val schreierGen = ubx |+| node.uInv(i)
          siftAndUpdateBaseFrom(node, schreierGen) match {
            case some: Some[(BSGSMutableNode[P], P)] => return some
            case _ =>
          }
        }
      }
    }
    None
  }
/*
  def completeStrongGenerators(implicit builder: BSGSMutableNodeBuilder): Unit = {
    def rec(node: BSGSNode[P])(implicit tb: TransversalBuilder): Unit = {
      findNewStrongGeneratorAt
    }
    start match {
      case _: BSGSTerm[P] =>
      case node: BSGSNode[P] = rec(findLastNode(node))
    }
  }*/
}
