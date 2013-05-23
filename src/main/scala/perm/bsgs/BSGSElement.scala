package com.faacets
package perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds

case class BSGSElement[E <: PermElement[E]](b: Dom, private[bsgs] nextEl: BSGSElement[E], g: BSGSGroup[E]) extends PermElement[BSGSElement[E]] {
  def *(that: BSGSElement[E]) = g.fromBaseImage(baseImage.map( k => that.image(k) ))
  def inverse = g.fromBaseImage(g.base.map( k => invImage(k) ))
  def toTeX = TeX(sequence.mkString("\\text{B}_{"," ","}"))
  def explicit = represents.explicit
  def isIdentity = (b == g.trv.beta) && nextElNotNullOr(nextEl.isIdentity, true)
  def compatible(that: BSGSElement[E]) = g.compatible(that)
  def size = g.trv(b)._1.size
  def equal(that: BSGSElement[E]): Boolean = (b == that.b) && nextElNotNullOr( nextEl.equal(that.nextEl), true)
  def image(k: Dom) = g.trv.u(b).image(nextElNotNullOr(nextEl.image(k), k))
  def invImage(k: Dom) = nextElNotNullOr(nextEl.invImage(g.trv.uinv(b).image(k)), g.trv.uinv(b).image(k))
  def images = represents.images
  def represents: E = nextElNotNullOr(nextEl.represents * g.trv.u(b), g.trv.u(b))
  def nextElNotNullOr[R](f: => R, v: R) = nextEl match {
    case null => v
    case _ => f
  }
  def sequence: List[Dom] = b :: nextElNotNullOr(nextEl.sequence, Nil)
  def baseImageHelper(img: Dom => Dom): List[Dom] =
    img(g.trv.beta) :: nextElNotNullOr(nextEl.baseImageHelper(img), Nil)
  def baseImage: List[Dom] =
    baseImageHelper(image)
}
