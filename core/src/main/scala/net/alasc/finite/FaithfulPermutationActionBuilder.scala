package net.alasc.finite

import spire.algebra.Action

import net.alasc.algebra.PermutationAction

trait FaithfulActionBuilder[G, @specialized(Int) P, A <: Action[P, G]] extends Function1[Iterable[G], A]
