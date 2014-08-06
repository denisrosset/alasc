package net.alasc.algebra

import scala.util.Random
import spire.algebra.Group
import spire.syntax.group._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.permutation._

/** Conjugate of an object `t` by a group element `g`. */
case class Conjugate[T, A: Group](g: A, t: T, gInv: A) {
  override def toString = Seq(g.toString, "|+|", t.toString, "|+|", gInv.toString).mkString(" ")
  @inline def iso(a: A): A = g |+| a |+| gInv
}

object Conjugate {
  def apply[T, A: Group](g: A, t: T): Conjugate[T, A] = Conjugate(g, t, g.inverse)
  implicit def ConjugateSubgroup[S, G](implicit sg: Subgroup[S, G], scalar: FiniteGroup[G]): Subgroup[Conjugate[S, G], G] = new ConjugateSubgroup[S, G]
  implicit def ConjugatePermutationSubgroup[S, P](implicit sg: PermutationSubgroup[S, P], scalar: Permutation[P]): PermutationSubgroup[Conjugate[S, P], P] =
    new ConjugatePermutationSubgroup[S, P]
}

class ConjugateSubgroup[S, G](implicit val sg: Subgroup[S, G], val scalar: FiniteGroup[G]) extends Subgroup[Conjugate[S, G], G] {
  type C = Conjugate[S, G]
  def elements(conj: C) = conj.t.elements.map(conj.iso)
  def generators(conj: C) = conj.t.generators.map(conj.iso)
  def order(conj: C) = conj.t.order
  def random(conj: C, gen: Random) = conj.iso(conj.t.random(gen))
}

class ConjugatePermutationSubgroup[S, P](implicit val sg: PermutationSubgroup[S, P], val scalar: Permutation[P])
    extends PermutationSubgroup[Conjugate[S, P], P] {
  type C = Conjugate[S, P]
  def elements(conj: C) = conj.t.elements.map(conj.iso)
  def generators(conj: C) = conj.t.generators.map(conj.iso)
  def order(conj: C) = conj.t.order
  def random(conj: C, gen: Random) = conj.iso(conj.t.random(gen))
}
