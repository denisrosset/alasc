package net.alasc
package std

import spire.algebra.Group
import spire.algebra.partial.PartialAction
import spire.util.Opt

import scalin.Vec
import scalin.algebra.VecEngine

import net.alasc.algebra._

final class VecPermutation[A, V <: Vec[A], G:PermutationAction:Group](implicit V: VecEngine[A, V]) extends PartialAction[V, G] {

  import net.alasc.syntax.permutationAction._
  import spire.syntax.action._
  import spire.syntax.group._

  override def actlIsDefined(g: G, v: V) = g.supportMax.getOrElseFast(-1) < v.length

  override def actrIsDefined(v: V, g: G) = g.supportMax.getOrElseFast(-1) < v.length

  def partialActl(g: G, v: V): Opt[V] =
    if (g.supportMax.getOrElseFast(-1) >= v.length) Opt.empty[V] else
      Opt(V.tabulate(v.length)( k => v(k <|+| g) ))

  def partialActr(v: V, g: G): Opt[V] = partialActl(g.inverse, v)

}

trait VecInstances {

  implicit def vecPermutation[A, V <: Vec[A], G:PermutationAction:Group](implicit V: VecEngine[A, V]): PartialAction[V, G] = new VecPermutation[A, V, G]

}

object vec extends VecInstances