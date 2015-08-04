package net.alasc
package math

import scala.language.implicitConversions

import scala.util.Random
import spire.algebra.{Action, Eq, Group}
import spire.syntax.group._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._

/** Conjugate of an object `t` by a group element `g`. */
case class Conjugate[G: FiniteGroup, T](t: T, g: G, gInv: G) {
  override def toString = Seq(gInv.toString, "|+|", t.toString, "|+|", g.toString).mkString(" ")
  def iso(a: G): G = gInv |+| a |+| g
}

object Conjugate {
  def apply[G: FiniteGroup, T](t: T, g: G): Conjugate[G, T] = Conjugate(t, g, g.inverse)
  implicit def ConjugateSubgroup[G, S](implicit sg: Subgroup[S, G]): Subgroup[Conjugate[G, S], G] = new ConjugateSubgroup[G, S]
  implicit def conjugateSubgroup[G, S](subgroup: S)(implicit sg: Subgroup[S, G]): Conjugate[G, S] =
    Conjugate(subgroup, sg.finiteGroup.id, sg.finiteGroup.id)(sg.finiteGroup)
}

class ConjugateGroupAction[G, S](implicit val sg: Subgroup[S, G]) extends Action[Conjugate[G, S], G] {
  import sg.{finiteGroup, eq}
  type C = Conjugate[G, S]
  def actl(g: G, conj: C) = Conjugate(conj.t, g |+| conj.g, conj.gInv |+| g.inverse)
  def actr(conj: C, gInv: G) = Conjugate(conj.t, gInv.inverse |+| conj.g, conj.gInv |+| gInv)
}

class ConjugateSubgroup[G, S](implicit val sg: Subgroup[S, G]) extends Subgroup[Conjugate[G, S], G] {
  implicit def equality: Eq[G] = sg.equality
  implicit def finiteGroup: FiniteGroup[G] = sg.finiteGroup
  type C = Conjugate[G, S]
  def iterator(conj: C) = conj.t.iterator.map(conj.iso)
  def generators(conj: C) = conj.t.generators.map(conj.iso)
  def order(conj: C) = conj.t.order
  def randomElement(conj: C, gen: Random) = conj.iso(conj.t.randomElement(gen))
  override def contains(conj: C, g: G) = conj.t.contains(conj.g |+| g |+| conj.gInv)
}
