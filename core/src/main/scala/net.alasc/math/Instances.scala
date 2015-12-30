package net.alasc.math

import spire.algebra.{Action, Group}

import net.alasc.algebra._

trait ConjugateInstances {

  implicit def ConjugateGroupAction[G, S](implicit sg: Subgroup[S, G], group: Group[G]): Action[Conjugate[G, S], G] = new ConjugateGroupAction[G, S]

}

trait AllInstances extends ConjugateInstances
