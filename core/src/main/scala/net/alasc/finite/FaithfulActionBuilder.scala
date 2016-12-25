package net.alasc.finite

import spire.algebra.Action

/** Constructs a faithful action of type A to represent a group. */
trait FaithfulActionBuilder[G, @specialized(Int) P, A <: Action[P, G]] extends Function1[Iterable[G], A] {

  /** Constructs a faithful action of type A given a collection of generators. */
  def apply(generators: Iterable[G]): A

  /** Constructs a faithful action of type A given a group. */
  def apply(grp: Grp[G]): A = apply(grp.generators)

}

object FaithfulActionBuilder {

  def apply[G, @specialized(Int) P, A <: Action[P, G]](implicit ev: FaithfulActionBuilder[G, P, A]): FaithfulActionBuilder[G, P, A] = ev

}
