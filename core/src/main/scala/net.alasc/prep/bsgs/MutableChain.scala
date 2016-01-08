package net.alasc.prep
package bsgs

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.action._
import spire.syntax.monoid._
import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._
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
    newNode.prev = prev
    newNode.next = next
    IsMutableNode.unapply(next).foreach { n => n.prev = newNode }
    // make node standalone
    IsMutableNode.unapply(node).foreach { n =>
      n.prev = null
      n.next = null
    }
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

  /** Prepares a base swap by replacing the two nodes with newly created nodes. In the new nodes,
    * the base points are swapped and previous valid generators are added.
    * 
    * Before: prev -> node1(beta1) -> node2(beta2) -> next
    * After:  prev ->  new1(beta2) ->  new2(beta1) -> next -- standalone node1(beta1), node2(beta2)
    * 
    * @return the two newly created nodes, and the orbit size goal (new1, new2, sizeGoal2).
    */
  def prepareSwap(prev: MutableStartOrNode[P], node1: MutableNode[P], node2: MutableNode[P], next: Chain[P])(
    implicit group: Group[P], nodeBuilder: NodeBuilder[P], classTag: ClassTag[P]): (MutableNode[P], MutableNode[P], BigInt) = {
    implicit def action = start.action
    require(prev.next eq node1)
    require(node1.prev eq prev)
    require(node1.next eq node2)
    require(node2.prev eq node1)
    require(node2.next eq next)
    IsMutableNode.unapply(next).foreach { n => require(n.prev eq node2) }
    // node2.ownGenerators are simply copied to newNode1 during the cloning, as they satisfy
    // (newNode1.beta <|+| g) != newNode1.beta
    val newNode1 = nodeBuilder.standaloneClone(node2)
    val newNode2 = nodeBuilder.standalone(node1.beta)
    node1.prev.next = newNode1
    newNode1.prev = node1.prev
    newNode1.next = newNode2
    newNode2.prev = newNode1
    newNode2.next = node2.next
    IsMutableNode.unapply(node2.next).foreach { n => n.prev = newNode2 }
    node1.prev = null
    node1.next = null
    node2.prev = null
    node2.next = null
    // node1.ownGenerators, on the other hand, can have either (newNode1.beta <|+| g) == newNode1.beta, in
    // which case they are copied to newNode2, or (newNode1.beta <|+| g) != newNode1.beta, in which case they
    // are copied to newNode1
    // first update newNode2.ownGeneratorsPairs, and keep aside the pairs for newNode1
    var chainIt = newNode2
    @tailrec def rec(current: Chain[P]): Unit = current match {
      case node: Node[P] =>
        cforRange(0 until node.nOwnGenerators) { i =>
          newNode2.updateTransversal(node.ownGenerator(i), node.ownGeneratorInv(i))
        }
        rec(node.next)
      case _: Term[P] =>
    }
    rec(newNode2)

    cforRange(0 until node1.nOwnGenerators) { i1 =>
      val g1 = node1.ownGenerator(i1)
      val gInv1 = node1.ownGeneratorInv(i1)
      if ((newNode1.beta <|+| g1) == newNode1.beta) {
        newNode2.addToOwnGenerators(g1, gInv1)
        newNode2.updateTransversal(g1, gInv1)
        newNode1.updateTransversal(g1, gInv1)
      } else {
        newNode1.addToOwnGenerators(g1, gInv1)
        newNode1.updateTransversal(g1, gInv1)
      }
    }
    val sizeGoal2 = ((node1.orbitSize.toLong * node2.orbitSize.toLong) / newNode1.orbitSize).toInt
    (newNode1, newNode2, sizeGoal2)
  }

  /** Returns a mutable node after `elem`, making it mutable from immutable, or inserting a new node it if terminal.
    * 
    * @param elem    Element for which to obtain a mutable `next`
    * @param beta    Base point for the new node to insert after `elem` if `elem` is the last node in the chain
    * 
    * @return the mutable node after `elem`.
    */  
  def mutableNodeAfter(elem: MutableStartOrNode[P], beta: => Int)(implicit group: Group[P], nodeBuilder: NodeBuilder[P], classTag: ClassTag[P]): MutableNode[P] = elem.next match {
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
    implicit builder: NodeBuilder[P], group: Group[P], classTag: ClassTag[P]): MutableNode[P] = {
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

  def makeFullyMutable(after: MutableStartOrNode[P] = start)(
    implicit builder: NodeBuilder[P], group: Group[P], classTag: ClassTag[P]): Unit =
    findLast(after) match {
      case node: Node[P] => mutable(node)
      case _ =>
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
          case start: Start[P] =>
            mutable.makeImmutable
            assert(start eq elem) // finished
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
    implicit builder: NodeBuilder[P], group: Group[P], classTag: ClassTag[P]): MutableNode[P] = {
    @tailrec def rec(prev: MutableStartOrNode[P]): MutableNode[P] = {
      val nextIsNode = prev.next eq node
      prev.next match {
        case IsMutableNode(mutableNext) =>
          if (nextIsNode)
            mutableNext
          else
            rec(mutableNext)
        case next: Node[P] =>
          val mutableNext = immutableToMutable(prev, next)
          if (nextIsNode)
            mutableNext
          else
            rec(mutableNext)
         case _: Term[P] => sys.error("Node cannot be found in the chain.")
      }
    }
    rec(after)
  }

  def mutableStartOrNode(startOrNode: StartOrNode[P], after: MutableStartOrNode[P] = start)(
    implicit builder: NodeBuilder[P], group: Group[P], classTag: ClassTag[P]): MutableStartOrNode[P] =
    startOrNode match {
      case node: Node[P] => mutable(node, after)
      case start: Start[P] => start
    }

  /** Removes the first node from the chain and returns it. If the chain is empty,
    * an empty node with given base point is created. */
  def detachFirstNode(beta: => Int)(implicit builder: NodeBuilder[P], group: Group[P], action: FaithfulPermutationAction[P], classTag: ClassTag[P]): Node[P] = start.next match {
    case IsMutableNode(mn) =>
      start.next = mn.next
      IsMutableNode.unapply(mn.next).foreach { n => n.prev = start }
      mn.prev = null
      mn.next = null
      mn
    case node: Node[P] =>
      start.next = node.next
      node
    case term: Term[P] =>
      builder.standalone(beta)
  }

  /** Converts the current chain to immutable.
    * 
    * @return the immutable chain.
    */
  def toChain(): Chain[P] = {
    makeImmutableAfter(start)
    start.next
  }

  def conjugate(g: P, gInv: P)(implicit group: Group[P], nodeBuilder: NodeBuilder[P], classTag: ClassTag[P]): Unit = {
    @tailrec def rec(prev: MutableStartOrNode[P]): Unit = prev.next match {
      case IsMutableNode(mn) =>
        mn.conjugate(g, gInv)
        rec(mn)
      case node: Node[P] =>
        val mutableNode = mutable(node, prev)
        mutableNode.conjugate(g, gInv)
        rec(mutableNode)
      case _: Term[P] => // finished
    }
    rec(start)
  }
}

final class MutableChainCheck[P:ClassTag:Eq:Group] extends Check[MutableChain[P]] {
  import Check._

  @tailrec def checkAllAction(checked: Checked, chain: Chain[P], action: FaithfulPermutationAction[P]): Checked = chain match {
    case node: Node[P] =>
      checkAllAction(checked ++ Check.eq(node.action, action, "Same action for all nodes"),
        node.next, action)
    case _: Term[P] => checked
  }

  @tailrec def checkMutablePrev(checked: Checked, elem: MutableStartOrNode[P]): Checked = elem.next match {
    case IsMutableNode(mutableNode) =>
      checkMutablePrev(checked ++ Check.eq(mutableNode.prev, elem, "Chain consistency"),
        mutableNode)
    case _ => checked
  }

  def check(mutableChain: MutableChain[P]): Checked =
    (checkAllAction(Check.success, mutableChain.start.next, mutableChain.start.action) ++
      checkMutablePrev(Check.success, mutableChain.start) ++
      mutableChain.start.next.check)

}

object MutableChain {

  /** Returns an empty mutable chain. */
  def empty[G:FaithfulPermutationAction:Group]: MutableChain[G] =
    new MutableChain(new Start(next = Term[G]))

  /** Returns a newly created empty mutable chain with the given base and action. */
  def emptyWithBase[G:ClassTag:Eq:FaithfulPermutationAction:Group](base: Seq[Int]): MutableChain[G] = {
    val mutableChain = MutableChain.empty[G]
    @tailrec def rec(prev: MutableStartOrNode[G], iterator: Iterator[Int]): Unit =
      if (iterator.hasNext) {
        val beta = iterator.next
        val newNode = NodeBuilder[G].standalone(beta)
        mutableChain.insertInChain(prev, prev.next, newNode)
        rec(newNode, iterator)
      }
    rec(mutableChain.start, base.iterator)
    mutableChain
  }

  def incompleteWithGenerators[G:ClassTag:Eq:FaithfulPermutationAction:Group:NodeBuilder](
    generators: Iterable[G], base: Seq[Int] = Seq.empty): MutableChain[G] = {
    val mutableChain = emptyWithBase(base)
    mutableChain.insertGenerators(generators)
    mutableChain
  }

  implicit def check[G:ClassTag:Eq:Group]: Check[MutableChain[G]] = new MutableChainCheck[G]

}
