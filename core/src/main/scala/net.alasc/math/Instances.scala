package net.alasc.math

import net.alasc.algebra._
import spire.algebra.GroupAction

trait ConjugateInstances {
  implicit def ConjugateGroupAction[G, S](implicit sg: Subgroup[S, G], scalar: FiniteGroup[G]): GroupAction[Conjugate[G, S], G] = new ConjugateGroupAction[G, S]
}

trait AllInstances extends ConjugateInstances
