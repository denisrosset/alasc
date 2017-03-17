package net.alasc.wreath

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

import spire.algebra._
import spire.algebra.partial._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.syntax.group._

class SeqEqGroup[SG <: SeqLike[G, SG], G:Eq:Group](implicit cbf: CanBuildFrom[Nothing, G, SG]) extends Eq[SG] with Group[SG] {

  def empty: SG = cbf().result
  def eqv(x: SG, y: SG): Boolean = {
    val xs = x.size
    val ys = y.size
    val s = xs.max(ys)
    var i = 0
    while (i < s){
      if (i >= xs) {
        if (!y(i).isId)
          return false
      } else if (i >= ys) {
        if (!x(i).isId)
          return false
      } else {
        if (x(i) =!= y(i))
          return false
      }
      i += 1
    }
    true
  }
  def inverse(sg: SG): SG = {
    val b = cbf()
    b.sizeHint(sg)
    sg.foreach { g => b += g.inverse }
    b.result
  }
  def combine(x: SG, y: SG): SG = {
    val b = cbf()
    b.sizeHint(x.size.max(y.size))
    val xi = x.iterator
    val yi = y.iterator
    var stay = true
    while (stay) {
      if (xi.hasNext) {
        if (yi.hasNext)
          b += xi.next |+| yi.next
        else
          b += xi.next
      } else {
        if (yi.hasNext)
          b += yi.next
        else
          stay = false
      }
    }
    b.result
  }

}

class SeqPermutationAction[SA <: SeqLike[A, SA], A, P:Group:PermutationAction](
  implicit cbf: CanBuildFrom[Nothing, A, SA]) extends PartialAction[SA, P] {

  import spire.syntax.action._
  import spire.syntax.group._

  import net.alasc.syntax.permutationAction._

  override def actlIsDefined(p: P, s: SA) = p.largestMovedPoint.getOrElseFast(-1) < s.length
  override def actrIsDefined(s: SA, p: P) = p.largestMovedPoint.getOrElseFast(-1) < s.length

  def partialActl(p: P, s: SA): Opt[SA] =
    if (p.largestMovedPoint.getOrElseFast(-1) >= s.length) Opt.empty[SA] else {
      val b = cbf()
      b.sizeHint(s)
      for (i <- 0 until s.length)
        b += s(i <|+| p)
      Opt(b.result)
    }

  def partialActr(s: SA, p: P): Opt[SA] = partialActl(p.inverse, s)

}
