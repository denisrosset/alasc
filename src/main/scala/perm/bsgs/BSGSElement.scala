package com.faacets
package perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds

sealed abstract class BSGSElement[E <: PermElement[E]] extends PermElement[BSGSElement[E]] {
  def b: Dom
  def g: BSGSGroup[E]
  def isTerminal: Boolean
  def tail: BSGSElement[E]
  def toTeX = TeX(sequence.mkString("\\text{B}_{"," ","}"))
  def inverse = g.fromBaseImage(g.base.map( k => invImage(k) ))
  def sequence: List[Dom]
  def baseImage: List[Dom] =
    baseImageHelper(image)
  def baseImageHelper(img: Dom => Dom): List[Dom]
  def images = represents.images
  def represents: E
}

final case class BSGSElementTerminal[E <: PermElement[E]](g: BSGSGroup[E]) extends BSGSElement[E] {
  def isTerminal = true
  def compatible(that: BSGSElement[E]) = equal(that)
  def size = g.representedIdentity.size
  def explicit = g.representedIdentity.explicit
  def b = throw new IllegalArgumentException("Invalid operation on end of BSGS chain.")
  def tail = throw new IllegalArgumentException("Invalid operation on end of BSGS chain.")
  def *(that: BSGSElement[E]) = that
  def represents = g.representedIdentity
  override def inverse = this
  def isIdentity = true
  def image(k: Dom) = k
  def invImage(k: Dom) = k
  def sequence = Nil
  def baseImageHelper(img: Dom => Dom) = Nil
  def equal(that: BSGSElement[E]) = that match {
    case BSGSElementTerminal(g1) => true
    case _ => false
  }
}

final case class BSGSElementNode[E <: PermElement[E]](g: BSGSGroup[E], b: Dom, private[bsgs] var tl: BSGSElement[E]) extends BSGSElement[E] {
  def isTerminal = false
  def tail = tl
  def *(that: BSGSElement[E]) = g.fromBaseImage(baseImage.map( k => that.image(k) )) // TODO: optimize
  def explicit = represents.explicit
  def isIdentity = (b == g.transversal.beta) && tail.isIdentity
  def compatible(that: BSGSElement[E]) = g.compatible(that)
  def size = g.representedIdentity.size
  def equal(that: BSGSElement[E]): Boolean = that match {
    case BSGSElementNode(g1, b1, tl1) => b == b1 && tl.equal(tl1)
    case _ => false
  }
  def image(k: Dom) = g.transversal.u(b).image(tail.image(k))
  def invImage(k: Dom) = tail.invImage(g.transversal.uinv(b).image(k))
  def represents: E = tail.represents * g.transversal.u(b)
  def sequence: List[Dom] = b :: tail.sequence
  def baseImageHelper(img: Dom => Dom): List[Dom] =
    img(g.transversal.beta) :: tail.baseImageHelper(img)
}
