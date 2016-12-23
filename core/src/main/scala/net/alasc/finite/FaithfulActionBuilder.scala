package net.alasc.finite

import spire.algebra.Action

trait FaithfulActionBuilder[G, @specialized(Int) P, A <: Action[P, G]] extends Function1[Iterable[G], A]

object FaithfulActionBuilder {

  def apply[G, @specialized(Int) P, A <: Action[P, G]](implicit ev: FaithfulActionBuilder[G, P, A]): FaithfulActionBuilder[G, P, A] = ev

}
