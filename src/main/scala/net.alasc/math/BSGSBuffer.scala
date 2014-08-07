package net.alasc.math

import spire.syntax.groupAction._
import spire.syntax.group._
import scala.annotation.tailrec
import net.alasc.algebra._
import net.alasc.syntax.permutation._
import net.alasc.syntax.subgroup._
import bsgs._

/** Builder for a BSGS chain.
  * 
  * Inspired by scala.collection.mutable.ListBuffer.
  */
final class BSGSBuffer[P](implicit val ev: Permutation[P]) {
  private var start: BSGS[P] = new BSGSTerm[P]
  private var last0: BSGSNode[P] = _
  private var exported: Boolean = false
  private var len = 0

  def chain: BSGS[P] = start
  def length = len

  /** Clears the buffer contents.
    */
  def clear() {
    start = new BSGSTerm[P]
    last0 = null
    exported = false
    len = 0
  }

  /** Copy contents of this buffer */
  private def copy() {
    var cursor = start
    val limit = last0.tail
    clear()
    while (cursor ne limit) {
      val node = cursor.asInstanceOf[BSGSNode[P]]
      append(node.transversal, node.ownGenerators)
      cursor = node.tail
    }
  }

  def append(beta: Int)(implicit tb: TransversalBuilder): this.type =
    append(tb.empty(beta), Nil)

  def prepend(beta: Int)(implicit tb: TransversalBuilder): this.type =
    prepend(tb.empty(beta), Nil)

  /** Appends a single transversal to this buffer.
   *
   *  @param tv  the transversal to append.
    * @param og  the strong generators to append. 
   *  @return    this BSGSBuffer.
   */
  def append(tv: Transversal[P], og: List[P]): this.type = {
    if (exported) copy()
    if (start.isTerminal) {
      last0 = new BSGSNode(tv, og, start)
      start = last0
    } else {
      val last1 = last0
      last0 = new BSGSNode(tv, og, last1.tail)
      last1.tl = last0
    }
    len += 1
    this
  }

  /** Prepends a single transversal to this buffer. This operation takes constant
   *  time.
   *
   * @param tv  the transversal to prepend.
   * @return    this BSGSBuffer.
   */
  def prepend(tv: Transversal[P], og: List[P]): this.type = {
    if (exported) copy()
    val newElem = new BSGSNode(tv, og, start)
    if (start.isTerminal) last0 = newElem
    start = newElem
    len += 1
    this
  }

  /** Adds the given generators to the BSGS chain, adding base elements
    * if necessary.
    * 
    * @param generators  Sequence of new generators. Must not contain the identity.
    */
  def addGenerators(generators: Seq[P])(implicit tb: TransversalBuilder): Unit = {
    if (generators.isEmpty) return
    if (exported) copy()
    @tailrec def rec(node: BSGSNode[P], remaining: Seq[P]): Unit = {
      val (generatorsThere, newRemaining) = remaining.partition(g => (node.beta <|+| g) != node.beta)
      node.og ++:= generatorsThere

      if (!newRemaining.isEmpty) {
        node.tail match {
          case _: BSGSTerm[P] =>
            val newBeta: Int = newRemaining.head.supportMin
            assert(newBeta != -1)
            append(newBeta)
            rec(node.tail.asInstanceOf[BSGSNode[P]], newRemaining)
          case tailNode: BSGSNode[P] =>
            rec(tailNode, newRemaining)
        }
      }
    }
    start match {
      case _: BSGSTerm[P] =>
        val newBeta = generators.head.supportMin
        assert(newBeta != -1)
        append(newBeta)
        addGenerators(generators)
      case node: BSGSNode[P] =>
        rec(node, generators)
    }
  }

  def addStrongGenerator(node: BSGSNode[P], p: P) {
    if (exported) copy()
    node.og = p :: node.og
    node.tv = node.tv.updated(Seq(p), node.strongGeneratingSet)
  }

  /** Updates the BSGS chain by adding the strong generator `p` at node `where`, and
    * updating the transversals from `start` to `where` included.
    * 
    * Note: assumes that this BSGSBuffer contains `where`.
    */
    def updateUpTo(where: BSGSNode[P], p: P) {
      if (exported) copy()

    where.og = p :: where.og
    @tailrec def updateRec(node: BSGSNode[P]) {
      node.tv = node.tv.updated(Seq(p), node.strongGeneratingSet)
      if (node ne where)
        updateRec(node.tail.asInstanceOf[BSGSNode[P]])
    }
    updateRec(start.asInstanceOf[BSGSNode[P]])
  }

  /** Sifts the element `p` through the BSGS chain, and returns either:
    * 
    * - `None` if `p` can be sifted completely,
    * - `Some(pair)` where `pair` describes the node and the
    *   (incompletely) sifted element that should be inserted up to it.
    * 
    * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
    */
  def siftAndAdd(p: P)(implicit tb: TransversalBuilder): Option[(BSGSNode[P], P)] = {
    if (exported) copy()
    @tailrec def rec(node: BSGSNode[P], remaining: P): Option[(BSGSNode[P], P)] = {
      val b = node.beta <|+| remaining
      if (!node.transversal.isDefinedAt(b)) {
        return Some(node -> remaining)
      }
      val h = remaining |+| node.transversal(b).gInv
      assert(node.beta <|+| h == node.beta) // TODO remove
      node.tail match {
        case _: BSGSTerm[P] if h.isId => None
        case _: BSGSTerm[P] =>
          val newBasePoint = h.supportMin
          assert(newBasePoint != -1) // there is a support for h =!= identity
          append(newBasePoint)
          Some(node.tail.asInstanceOf[BSGSNode[P]] -> h)
        case tailNode: BSGSNode[P] =>
          rec(tailNode, h)
      }
    }

    start match {
      case _: BSGSTerm[P] =>
        val beta = p.supportMin
        if (beta == -1) // p is the identity
          None
        else {
          append(beta)
          siftAndAdd(p)
        }
      case node: BSGSNode[P] =>
        rec(node, p)
    }
  }

  def putInOrder(bsgs: BSGS[P]): Unit = {
    if (exported) copy()

  }

  def toBSGS: BSGS[P] = {
    exported = !start.isTerminal
    start
  }
}
