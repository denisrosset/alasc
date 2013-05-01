package com.faacets
package perm

import scala.util.Random

object Perm {
  def apply(n: Int) = new Perm((0 until n).toArray)
  def fromImages(imgs: Domain*) = new Perm(imgs.map(_.zeroBased).toArray)
}

class Perm(val arr: Array[Int]) extends AnyVal with PermElement[Perm] {
  def isIdentity: Boolean = domain.forall( k => k == image(k) )
  def size = arr.size
  override def toString = "Perm("+size+")"+cycles.filter(_.size>1).map(_.mkString("(",",",")")).mkString("")
  def image(k: Domain) = Domain.zeroBased(arr(k.zeroBased))
  def images0: ArrayDomain0 = arr.clone
  def images: ArrayDomain1 = Array.tabulate[Int](size)(arr(_)+1)
  def compatible(that: Perm) = size == that.size
  def explicit = this
  def compare(that: Perm) = {
    require_(compatible(that))
    val firstNotEqual = domain.find(k => image(k) != that.image(k))
    firstNotEqual match {
      case None => 0
      case Some(k) if image(k) <= that.image(k) => -1
      case _ => 1
    }
  }
  // note that the image of b under the product g*h is given by:
  // b^(g*h) = (b^g)^h
  def *(that: Perm): Perm = {
    require_(compatible(that))
    val a = new Array[Int](size)
    for (i <- 0 until size) a(i) = that.arr(arr(i))
    new Perm(a)
  }
  def inverse: Perm = {
    val a = new Array[Int](size)
    for (i <- 0 until size) a(arr(i)) = i
    new Perm(a)
  }
  def equal(that: Perm) = {
    require_(compatible(that))
    arr.sameElements(that.arr)
  }
  def withSwap(i: Domain, j: Domain): Perm = {
    require_(isDefinedAt(i) && isDefinedAt(j))
    val a = arr.clone
    val k = a(i.zeroBased)
    a(i.zeroBased) = a(j.zeroBased)
    a(j.zeroBased) = k
    new Perm(a)
  }
  def apply(cycle: Domain*) = {
    val a = arr.clone
    val a0 = a(cycle(0).zeroBased)
    for (i <- 0 until cycle.size - 1)
      a(cycle(i).zeroBased) = a(cycle(i + 1).zeroBased)
    a(cycle(cycle.size-1).zeroBased) = a0
    new Perm(a)
  }
}
