package net.alasc.math

import net.alasc.algebra._
import spire.algebra.Action

trait ConjugateInstances {
  implicit def ConjugateGroupAction[G, S](implicit sg: Subgroup[S, G], scalar: FiniteGroup[G]): Action[Conjugate[G, S], G] = new ConjugateGroupAction[G, S]
}

trait AllInstances extends ConjugateInstances
