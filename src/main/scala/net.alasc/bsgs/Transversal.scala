package net.alasc
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds

sealed abstract class Transversal[E <: PermElement[E]] {
  def isTerminal: Boolean
  def tail: Transversal[E]

  def elements: Iterator[E]

  def representatives: List[E]
  def sizes: List[Int]
}

private[bsgs] final case class TransversalTerminal[E <: PermElement[E]](val id: E) extends Transversal[E] {
  def isTerminal = true
  def tail = throw new IllegalArgumentException("Cannot get tail of Transversal terminal.")

  def elements: Iterator[E] = Iterator(id)
  def representatives = Nil
  def sizes = Nil
}

private[bsgs] final case class TransversalNode[E <: PermElement[E]](val uList: List[E], val tl: Transversal[E]) extends Transversal[E] {
  def isTerminal = false
  def tail = tl
  def elements: Iterator[E] = for {
    u <- uList.iterator
    ne <- tail.elements
  } yield ne * u
  def representatives = uList
  def sizes = uList.size :: tail.sizes
}
