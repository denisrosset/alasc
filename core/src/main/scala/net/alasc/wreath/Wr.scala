package net.alasc.wreath

import scala.collection.immutable.SortedMap

import spire.algebra._
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.perms.{Cycle, Perm}
import net.alasc.rep.FaithfulPermRepBuilder
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.group._

/** Describes the wreath product of two objects. */
class Wr[A] protected[wreath] (val aMap: SortedMap[Int, A], val h: Perm) {

  def a(index: Int)(implicit A: Group[A]) = aMap.getOrElse(index, A.id)

  def n: Int = spire.math.max(aMap.keys.fold(-1)(spire.math.max), h.largestMovedPoint.getOrElseFast(-1)) + 1

  override def toString = s"Wr(" + aMap.map { case (k ,v) => s"$k -> $v" }.mkString(", ") + ")" + h.toCycles.string

  override def equals(any: Any) = any match {
    case that: Wr[_] => this.aMap == that.aMap && this.h === that.h
    case _ => false
  }

  override def hashCode = aMap.hashCode * 41 + h.hashCode()

  def apply(cycle: Int*): Wr[A] = new Wr(aMap, h(cycle: _*))

}

/** Default wreath product object and type classes for wreath products. */
object Wr {

  def apply[A:Eq:Group](base: (Int, A)*)(cycle: Int*): Wr[A] = {
    val aMap = SortedMap(base.filterNot(_._2.isId): _*)
    new Wr(aMap, Perm(cycle: _*))
  }

  def fromPerm[A:Eq:Group](base: (Int, A)*)(perm: Perm): Wr[A] = {
    val aMap = SortedMap(base.filterNot(_._2.isId): _*)
    new Wr(aMap, perm)
  }

  implicit def wrFaithfulPermutationActionBuilder[A:Eq:Group:FaithfulPermutationActionBuilder]: FaithfulPermutationActionBuilder[Wr[A]] =
    new WrFaithfulPermutationActionBuilder[A]

  class WrEqGroup[A:Eq:Group] extends Eq[Wr[A]] with Group[Wr[A]] {

    val aMapEq = spire.std.map.MapEq[Int, A]

    def eqv(x: Wr[A], y: Wr[A]): Boolean = (x.h === y.h) && aMapEq.eqv(x.aMap, y.aMap)
    def empty = Wr[A]()()
    def inverse(w: Wr[A]): Wr[A] = {
      val hInv = w.h.inverse
      val n = w.aMap.keys.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
      Wr.fromPerm(w.aMap.toSeq.map { case (i, a) => (i <|+| w.h, a.inverse) }: _*)(hInv)
    }
    def combine(x: Wr[A], y: Wr[A]): Wr[A] = {
      val newH = x.h |+| y.h
      val n = spire.math.max(x.n, y.n)
      Wr.fromPerm(Seq.tabulate(n)( i => (i, x.a(i) |+| y.a(i <|+| x.h) )): _*)(newH)
    }

  }

  implicit def wrEqGroup[A:Eq:Group]: Eq[Wr[A]] with Group[Wr[A]] = new WrEqGroup[A]

  def grpDef[A:Eq:Group](n: Int, ga: Grp[A], gh: Grp[Perm]): GrpDef[Wr[A]] = {
    val aGenerators = for {
      k <- 0 until n
      a <- ga.generators
    } yield Wr(k -> a)()
    val hGenerators = for {
      h <- gh.generators
    } yield Wr.fromPerm[A]()(h)
    val order = ga.order.pow(n) * gh.order
    GrpDef(aGenerators ++ hGenerators, Opt(order))
  }

}
