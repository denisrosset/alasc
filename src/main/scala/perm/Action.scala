package com.faacets.perm

import com.faacets.math._

trait Action[P <: GroupElement[P], A <: Action[P, A]] extends Permutation[A] {
  val g: P
  override def image(el: Domain) = images(el)
  def assertValid { g.assertValid }
  override def toString: String = this.getClass.getName + "(" + g.toString + ")"
  override def compare(that: A): Int = {
    import scala.math.Ordering.Implicits._
    Ordering[Vector[Int]].compare(images, that.images)
  }
  def build(p1: P): A
  def isIdentity = g.isIdentity
  def *(that: A) = build(g*that.g)
  def inverse = build(g.inverse)
  def equal(that: A) = g.equal(that.g)
}
