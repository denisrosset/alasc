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
      this += (node.transversal, node.ownGenerators)
      cursor = node.tail
    }
  }

  def +=(beta: Int)(implicit tb: TransversalBuilder): this.type =
    this.+=(tb.empty(beta), Nil)

  def +=:(beta: Int)(implicit tb: TransversalBuilder): this.type =
    this.+=:(tb.empty(beta), Nil)

  /** Appends a single transversal to this buffer.
   *
   *  @param tv  the transversal to append.
    * @param og  the strong generators to append. 
   *  @return    this BSGSBuffer.
   */
  def +=(tv: Transversal[P], og: List[P]): this.type = {
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
  def +=: (tv: Transversal[P], og: List[P]): this.type = {
    if (exported) copy()
    val newElem = new BSGSNode(tv, og, start)
    if (start.isTerminal) last0 = newElem
    start = newElem
    len += 1
    this
  }

  def appendAndExport(bsgs: BSGS[P]) = ???

  def addStrongGenerator(node: BSGSNode[P], p: P) {
    if (exported) copy()
    node.og = p :: node.og
    node.tv = node.tv.updated(Seq(p), node.strongGeneratingSet)
  }

  /** Adds the element `p` to the BSGS chain, if the element is not already
    * described.
    */
  def addElement(p: P)(implicit tb: TransversalBuilder): Option[P] = {
    if (exported) copy()
    @tailrec def rec(node: BSGSNode[P], remaining: P): Option[P] = {
      val b = node.beta <|+| remaining
      if (!node.transversal.isDefinedAt(b)) {
        addStrongGenerator(node, remaining)
        return Some(remaining)
      }
      val h = remaining |+| node.transversal(b).gInv
      assert(node.beta <|+| h == node.beta) // TODO remove
      node.tail match {
        case _: BSGSTerm[P] if h.isId => None
        case _: BSGSTerm[P] =>
          val newBasePoint = h.supportMin
          assert(newBasePoint != -1) // there is a support for h =!= identity
          this.+=(newBasePoint)
          addStrongGenerator(node.tail.asInstanceOf[BSGSNode[P]], h)
          Some(h)
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
          this.+=(beta)
          addElement(p)
        }
      case node: BSGSNode[P] =>
        rec(node, p)
    }
  }

  def toBSGS: BSGS[P] = {
    exported = !start.isTerminal
    start
  }
}
