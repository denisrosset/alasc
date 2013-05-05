package com.faacets
package perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random

abstract class BSGS[G <: PermGroup[E], E <: PermElement[E]] {
  type Transversal <: AbstractTransversal[_, E]
}

private[bsgs] trait BSGSLike[E <: PermElement[E]] {
  def trv: TransversalLike[E]
  private[bsgs] def sgList: List[E]
  private[bsgs] def next: BSGSLike[E]
  def nextNotNullOr[Result](f: => Result, inCaseOfNull: Result): Result = next match {
    case null => inCaseOfNull
    case _ => f
  }

  @tailrec final def length(acc: Int = 1): Int = next match {
    case null => acc
    case _ => next.length(acc + 1)
  }
  def contains(el: E) = basicSift(el)._2.isIdentity
  def uSize: List[Int] = nextNotNullOr(trv.size :: next.uSize, trv.size :: Nil)
  def order: BigInt = nextNotNullOr(trv.size * next.order, trv.size)

  def basicSift(el: E): (List[Dom], E) = {
    val b = el.image(trv.beta)
    if (!trv.isDefinedAt(b))
      return (Nil, el)
    val nextEl = el * trv.uinv(b)
    next match {
      case null => (b :: Nil, el)
      case _ => {
        val (bList, retEl) = next.basicSift(nextEl)
        (b :: bList, retEl)
      }
    }
  }
}

private[bsgs] abstract class BSGSConstruction[T <: AbstractTransversal[T, E], E <: PermElement[E]](
  var trv: T,
  private[bsgs] var sgList: List[E],
  private[bsgs] var next: BSGSConstruction[T, E]) extends BSGSLike[E] with TransversalMixin[T, E] {

  def makeEmptyConstruction(t: T)

  def addStrongGenerator(h: E) {
    sgList = h :: sgList
    trv = trv.addingGenerator(h)
  }

  def construct(el: E, id: E): Option[E] = {
    val b = el.image(trv.beta)
    if (!trv.isDefinedAt(b)) {
      addStrongGenerator(el)
      return Some(el)
    }
    val h = el * trv.uinv(b)
    assert(h.image(trv.beta) == trv.beta)
    if (next eq null) {
      if (h.isIdentity)
        return None
      val newBase = (0 until el.size).find( k => h.image(Dom._0(k)) != Dom._0(k) ).get
      val newTransversal = makeEmptyTransversal(Dom._0(newBase), id)
      next = new BSGSConstruction(newTransversal, Nil, null)
      next.addStrongGenerator(h)
      addStrongGenerator(h)
      return Some(h)
    } else {
      next.construct(h, id) match {
        case None => return None
        case Some(gen) => {
          addStrongGenerator(gen)
          return Some(gen)
        }
      }
    }
  }

  def +=(el: E) = construct(el, el*el.inverse)
  def asGroup: BSGSGroup[T, E] = BSGSGroup(trv, sgList, nextNotNullOr(next.asGroup, null))
}


case class BSGSGroup[T <: AbstractTransversal[T, E], E <: PermElement[E]](
  val trv: T,
  private[bsgs] val sgList: List[E],
  private[bsgs] val next: BSGSGroup[T, E]) extends BSGSLike[E] with PermGroup[E] {
}

case class BSGSElement[E <: PermElement[E]](b: Dom, private[bsgs] nextEl: BSGSElement[E], g: BSGSLike[E]) {

}
