package net.alasc
package math

import net.alasc.algebra._
import scala.util.Random
import spire.algebra.{Group, GroupAction}
import spire.syntax.group._
import net.alasc.syntax.subgroup._
import scala.language.implicitConversions

/** Conjugate of an object `t` by a group element `g`. */
case class Conjugate[G: FiniteGroup, T](gInv: G, t: T, g: G) {
  override def toString =
    if (g.isId) t.toString else Seq(gInv.toString, "|+|", t.toString, "|+|", g.toString).mkString(" ")
  def iso(a: G): G = gInv |+| a |+| g
}

object Conjugate {
  def apply[G: FiniteGroup, T](g: G, t: T): Conjugate[G, T] = Conjugate(g.inverse, t, g)
  def apply[G: FiniteGroup, T](ip: InversePair[G], t: T): Conjugate[G, T] = Conjugate(ip.gInv, t, ip.g)
  implicit def ConjugateSubgroup[G, S](implicit sg: Subgroup[S, G], algebra: FiniteGroup[G]): Subgroup[Conjugate[G, S], G] = new ConjugateSubgroup[G, S]
  implicit def conjugateSubgroup[G, S](subgroup: S)(implicit sg: Subgroup[S, G], algebra: FiniteGroup[G]): Conjugate[G, S] =
    Conjugate(algebra.id, subgroup, algebra.id)
}

class ConjugateGroupAction[G, S](implicit val sg: Subgroup[S, G], val algebra: FiniteGroup[G]) extends GroupAction[Conjugate[G, S], G] {
  type C = Conjugate[G, S]
  def actl(g: G, conj: C) = Conjugate(conj.gInv |+| g.inverse, conj.t, g |+| conj.g)
  def actr(conj: C, gInv: G) = Conjugate(conj.gInv |+| gInv, conj.t, gInv.inverse |+| conj.g)
}

class ConjugateSubgroup[G, S](implicit val sg: Subgroup[S, G], val algebra: FiniteGroup[G]) extends Subgroup[Conjugate[G, S], G] {
  type C = Conjugate[G, S]
  def iterator(conj: C) = conj.t.iterator.map(conj.iso)
  def generators(conj: C) = conj.t.generators.map(conj.iso)
  def order(conj: C) = conj.t.order
  def randomElement(conj: C, gen: Random) = conj.iso(conj.t.randomElement(gen))
  override def contains(conj: C, g: G) = conj.t.contains(conj.g |+| g |+| conj.gInv)
}
