package com.faacets.perm

import Implicits._
import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.immutable

case class ExplicitPermutation(I: Vector[Domain]) extends RichPermutation[ExplicitPermutation] {
  override def compare(that: ExplicitPermutation): Int = {
    import scala.math.Ordering.Implicits._
    Ordering[Vector[Int]].compare(I, that.I)
  }
  override def images = I
  override def domainSize = I.size
  override def image(el: Domain) = I(el)
  // Standard scala methods
  override def toString = {
    if(ExplicitPermutation.printCycles) {
      def cycleStr(i: (Domain, Int)): String = cycle(i._1).mkString("(", ", ", ")")
      val cyclesStr = cycles(false).map(cycleStr(_)).mkString("","","")
      this.getClass.getName + "(" + domainSize + ")" + cyclesStr
    } else
      images.mkString(this.getClass.getName + "(",", ",")")
  }
  def *(other: ExplicitPermutation): ExplicitPermutation = {
    assert(domainSize == other.domainSize)
    new ExplicitPermutation(Vector(images.map(other.images(_)):_*))
  }
  def inverse: ExplicitPermutation = {
    val a = Array.fill[Domain](domainSize)(0)
    for (i <- 0 until domainSize) a(image(i)) = i
    new ExplicitPermutation(Vector(a:_*))
  }
  def apply(cycle: Domain*): ExplicitPermutation = {
    val P: Array[Int] = Array.tabulate[Domain](domainSize)((i:Domain) => i)
    val list = cycle.toList
    var el = list
    while (!el.tail.isEmpty) {
      P(el.head) = el.tail.head
      el = el.tail
    }
    P(el.head) = list.head
    this*new ExplicitPermutation(Vector(P:_*))
  }
  def resizedTo(n: Int): Option[ExplicitPermutation] = {
    if (n == domainSize) return Some(this)
    if (n > domainSize) return Some(new ExplicitPermutation(images ++ (domainSize until n)))
    if (n < domainSize && (n until domainSize).exists(hasInSupport(_)))
      return None
    return Some(new ExplicitPermutation(images.take(n)))
  }
}

object ExplicitPermutation {
  var printCycles = true
}
