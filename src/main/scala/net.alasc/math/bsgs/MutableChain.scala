package net.alasc.math
package bsgs

import scala.util.Random
import scala.annotation.tailrec
import scala.collection.mutable.{BitSet => MutableBitSet}
import scala.collection.immutable.{BitSet => ImmutableBitSet}

import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.check._
import net.alasc.util._

/** Mutable builder for a BSGS chain.
  * 
  * A MutableChain can be in three different states:
  * 
  * - fully immutable: no node is mutable, mutable operations will copy nodes as needed,
  * - partly mutable: the beginning of the chain is mutable, while the end is immutable,
  * - fully mutable: the whole chain is mutable.
  * 
  * The design allows structural sharing as long at it is possible, because only
  * nodes that are mutated are copied.
  * 
  * Mutable nodes are identified because their `prev` reference is initialized so that the
  * mutable part of the BSGS chain is a double-linked list.
  * 
  * If a node is mutable, then its `prev` is not null, and either points to this node's parent
  * which is either a node or a `Start` instance`.
  *
  * Moreover, if a node is mutable, all its parents must be mutable.
  * 
  * If the node is immutable, then `prev` is `null`.
  * 
  * The `PermutationAction` instances used with `MutableChain` should have an `equals` method where
  * `p1 == p2` if and only if `p1.actr(k, g) == p2.actr(k, g)` for all `k` and `g`.
  * 
  * `MutableChain` is not thread-safe.
  */
class MutableChain[P](val start: Start[P]) extends AnyVal { // TODO: ensure that all algebra in the chain are x eq y, not only x == y
  override def toString = start.toString

  /** Insert `node` in the chain between `prev` and `next`.
    * 
    * Before: `prev` -> `next`
    * After: `prev` -> `node` -> `next`
    * 
    * @param prev  Previous mutable node or start element.
    * @param next  Next element, can be an (im)mutable node or a terminal.
    * @param node  Standalone mutable node to insert, with the same (eq) action as the mutable chain.
    */
  def insertInChain(prev: MutableStartOrNode[P], next: Chain[P], node: MutableNode[P]): Unit = {
    require(node.action eq start.action)
    require(node.isStandalone)
    // prev and next must be immediate neighbors
    require(prev.next eq next)
    IsMutableNode.unapply(next).foreach { n => require(n.prev eq prev) }
    // insert the node in the list
    prev.next = node
    node.prev = prev
    node.next = next
    IsMutableNode.unapply(next).foreach { _.prev = node }
  }

  /** Replace `node` by `newNode`, between `prev` and `next`.
    * 
    * Before: `prev` -> `node` -> `next`
    * After: `prev` -> `newNode` -> `next`
    * 
    * @param prev    Previous mutable node or start element.
    * @param node    Im(mutable) node to replace.
    * @param next    Next (im)mutable node or terminal.
    * @param newNode Standalone mutable node to replace `node` by.
    */ 
  def replaceInChain(prev: MutableStartOrNode[P], node: Node[P], next: Chain[P], newNode: MutableNode[P]): Unit = {
    require(newNode.action eq start.action)
    require(newNode.isStandalone)
    // require prev -> node -> next
    require(prev.next eq node)
    IsMutableNode.unapply(node).foreach { n => require(n.prev eq prev) }
    require(node.next eq next)
    IsMutableNode.unapply(next).foreach { n => require(n.prev eq node) }
    prev.next = newNode
  }

  /** Remove `node`, located between `prev` and `next`.
    * 
    * Before: `prev` -> `node` -> `next`
    * After: `prev` -> `next`
    * 
    * @param prev Previous mutable node or start element.
    * @param node Im(mutable) node to remove, with orbit size = 1, and empty `ownGenerators`.
    * @param next Next (im)mutable node or terminal.
    */
  def remove(prev: MutableStartOrNode[P], node: Node[P], next: Chain[P]): Unit = {
    require(node.orbitSize == 1)
    require(node.ownGeneratorsPairs.isEmpty)
    // remove node from chain
    prev.next = next
    IsMutableNode.unapply(next).foreach { n => n.prev = prev }
    // if node is mutable, make it standalone
    IsMutableNode.unapply(node).foreach { n => n.prev = null; n.next = null }
  }

  /** Replace the chain after `prev` by another chain given after `newStart`.
    *
    * Before: prev -> chain1
    *         newStart -> chain2
    * After : prev -> chain2
    *         newStart invalid
    * 
    * @param prev     Element whose next chain will be replaced.
    * @param newStart Start element whose next elements provide the new chain.
    * 
    */
  def replaceChain(prev: MutableStartOrNode[P], newStart: Start[P]): Unit = {
    require(newStart.action eq start.action)
    // invalidate the previous chain
    IsMutableNode.unapply(prev.next).foreach { n => n.prev = null }
    // attach the new chain
    prev.next = newStart.next
    IsMutableNode.unapply(newStart.next).foreach { n => n.prev = prev }
    // invalidate the other start
    newStart.next = null
  }

  /** Returns a mutable node after `elem`, making it mutable from immutable, or inserting a new node it if terminal.
    * 
    * @param elem    Element for which to obtain a mutable `next`
    * @param beta    Base point for the new node to insert after `elem` if `elem` is the last node in the chain
    * 
    * @return the mutable node after `elem`.
    */  
  def mutableNodeAfter(elem: MutableStartOrNode[P], beta: => Int)(implicit algebra: FiniteGroup[P], nodeBuilder: NodeBuilder[P]): MutableNode[P] = elem.next match {
    case IsMutableNode(mutableNode) => mutableNode
    case immutableNode: Node[P] => immutableToMutable(elem, immutableNode)
    case term: Term[P] =>
      implicit def action = start.action
      val mutableNode = nodeBuilder.standalone(beta)
      insertInChain(elem, term, mutableNode)
      mutableNode
  }

  /** Makes `immutableNode`, located after `prev` mutable.
    * 
    * @param prev          Previous element to `immutableNode`.
    * @param immutableNode Immutable node to replace by a mutable copy.
    * @return the mutable copy of `immutableNode`
    */
  def immutableToMutable(prev: MutableStartOrNode[P], immutableNode: Node[P])(
    implicit builder: NodeBuilder[P], ev: FiniteGroup[P]): MutableNode[P] = {
    require(immutableNode.isImmutable)
    val mutableNode = builder.standaloneClone(immutableNode)
    replaceInChain(prev, immutableNode, immutableNode.next, mutableNode)
    mutableNode
  }

  /** Finds the last mutable node in the chain starting at `current`. */
  @tailrec def findLastMutable(current: MutableStartOrNode[P] = start): MutableStartOrNode[P] = current.next match {
    case IsMutableNode(mutableNext) => findLastMutable(mutableNext)
    case _ => current
  }

  /** Finds the last node/start in the chain starting at `current`. */
  @tailrec def findLast(current: StartOrNode[P] = start): StartOrNode[P] = current.next match {
    case nextNode: Node[P] => findLast(nextNode)
    case _: Term[P] => current
  }

  /** Makes the chain starting after `elem` fully immutable.
    * 
    * @param elem  Element with tail to be made immutable. Default = `start`, which will
    *              make the whole chain immutable.
    */
  def makeImmutableAfter(elem: MutableStartOrNode[P] = start): Unit = {
    @tailrec def rec(mutable: MutableNode[P]): Unit = {
      if (mutable ne elem) {
        mutable.prev match {
          case IsMutableNode(mutablePrev) =>
            mutable.makeImmutable
            rec(mutablePrev)
          case _: Node[P] => sys.error("An immutable node cannot be before a mutable node.")
          case start: Start[P] => assert(start eq elem) // finished
        }
      }
    }
    findLastMutable(elem) match {
      case IsMutableNode(mn) => rec(mn)
      case _: Start[P] => // nothing to do, chain is empty
    }
  }

  /** Makes the chain up to `node` mutable including it. Has no side effect if `node` is already mutable.
    * 
    * @param node  Node to replace with a mutable copy if immutable.
    * @param after Optional mutable parent of `node`.
    * 
    * @return the mutable node corresponding `node`
    */
  def mutable(node: Node[P], after: MutableStartOrNode[P] = start)(
    implicit builder: NodeBuilder[P], ev: FiniteGroup[P]): MutableNode[P] = {
    @tailrec def rec(prev: MutableStartOrNode[P]): MutableNode[P] = prev.next match {
      case IsMutableNode(mutableNext) =>
        if (mutableNext eq node)
          mutableNext
        else
          rec(mutableNext)
      case next: Node[P] =>
        immutableToMutable(prev, next)
        rec(prev)
      case _: Term[P] => sys.error("Node cannot be found in the chain.")
    }
    rec(after)
  }

  def mutableStartOrNode(startOrNode: StartOrNode[P], after: MutableStartOrNode[P] = start)(
    implicit builder: NodeBuilder[P], ev: FiniteGroup[P]): MutableStartOrNode[P] =
    startOrNode match {
      case node: Node[P] => mutable(node, after)
      case start: Start[P] => start
    }


  /** Converts the current chain to immutable.
    * 
    * @return the immutable chain.
    */
  def toChain: Chain[P] = {
    makeImmutableAfter(start)
    start.next
  }


/*
  def conjugate(mutableNode: MutableNode[P], newBeta: Int): Mutable[P] = {

  }

  /** Conjugates the BSGS chain using the given group element. */
  def conjugate(ip: InversePair[P]): this.type = {
    if (!isEmpty) {
      makeFullyMutable(sys.error("Cannot be empty"))
      foreachMutable { _.conjugateThisNode(ip) }
    }
    this
  }*/
}

object MutableChain {
  def empty[P: FiniteGroup](implicit action: PermutationAction[P]): MutableChain[P] = new MutableChain(new Start(next = Term[P]))
  implicit def MutableChainCheck[P: FiniteGroup]: Check[MutableChain[P]] = new MutableChainCheck[P]
}

final class MutableChainCheck[P](implicit val algebra: FiniteGroup[P]) extends Check[MutableChain[P]] {
  @tailrec def checkAllAction(chain: Chain[P], action: PermutationAction[P]): Unit = chain match {
    case node: Node[P] =>
      assert(node.action eq action)
      checkAllAction(node.next, action)
    case _: Term[P] =>
  }

  @tailrec def checkMutablePrev(elem: MutableStartOrNode[P]): Unit = elem.next match {
    case IsMutableNode(mutableNode) =>
      require(mutableNode.prev eq elem)
      checkMutablePrev(mutableNode)
    case _ =>
  }

  def check(mutableChain: MutableChain[P]): Unit = {
    checkAllAction(mutableChain.start.next, mutableChain.start.action)
    checkMutablePrev(mutableChain.start)
    mutableChain.start.next.check
  }
}
