package net.alasc.math

import net.alasc.algebra._
import scala.util.Random
import spire.algebra.{Group, GroupAction}
import spire.syntax.group._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.permutation._
import scala.language.implicitConversions

/** Conjugate of an object `t` by a group element `g`. */
case class Conjugate[G: FiniteGroup, T](g: G, t: T, gInv: G) {
  override def toString =
    if (g.isId) t.toString else Seq(g.toString, "|+|", t.toString, "|+|", gInv.toString).mkString(" ")
  @inline def iso(a: G): G = g |+| a |+| gInv
}

object Conjugate {
  def apply[G: FiniteGroup, T](g: G, t: T): Conjugate[G, T] = Conjugate(g, t, g.inverse)
  def apply[G: FiniteGroup, T](ip: InversePair[G], t: T): Conjugate[G, T] = Conjugate(ip.g, t, ip.gInv)
  implicit def ConjugateSubgroup[G, S](implicit sg: Subgroup[S, G], algebra: FiniteGroup[G]): Subgroup[Conjugate[G, S], G] = new ConjugateSubgroup[G, S]
  implicit def conjugateSubgroup[G, S](subgroup: S)(implicit sg: Subgroup[S, G], algebra: FiniteGroup[G]): Conjugate[G, S] =
    Conjugate(algebra.id, subgroup, algebra.id)
}

class ConjugateGroupAction[G, S](implicit val sg: Subgroup[S, G], val algebra: FiniteGroup[G]) extends GroupAction[Conjugate[G, S], G] {
  type C = Conjugate[G, S]
  def actl(g: G, conj: C) = Conjugate(g |+| conj.g, conj.t, conj.gInv |+| g.inverse)
  def actr(conj: C, gInv: G) = Conjugate(gInv.inverse |+| conj.g, conj.t, conj.gInv |+| gInv)
}

class ConjugateSubgroup[G, S](implicit val sg: Subgroup[S, G], val algebra: FiniteGroup[G]) extends Subgroup[Conjugate[G, S], G] {
  type C = Conjugate[G, S]
  def elements(conj: C) = conj.t.elements.map(conj.iso)
  def generators(conj: C) = conj.t.generators.map(conj.iso)
  def order(conj: C) = conj.t.order
  def random(conj: C, gen: Random) = conj.iso(conj.t.random(gen))
}
