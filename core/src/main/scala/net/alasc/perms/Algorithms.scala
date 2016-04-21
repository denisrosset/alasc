package net.alasc.perms

import scala.reflect.ClassTag
import scala.util.Random

import spire.NoImplicit
import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.Permutation
import net.alasc.bsgs.{BaseChange, BaseSwap, SchreierSims}
import net.alasc.finite.{GrpBuilder, Rep}
import net.alasc.bsgs.SchreierSims

class Algorithms(
                  val randomOpt: Opt[Random] = Opt(Random),
                  val baseChangeRecomputes: Boolean = false,
                  val baseChangeConjugates: Boolean = true) {

  implicit val schreierSims: SchreierSims = randomOpt match {
    case Opt(random) => SchreierSims.randomized(random)
    case _ => SchreierSims.deterministic
  }

  implicit val baseSwap: BaseSwap = randomOpt match {
    case Opt(random) => BaseSwap.randomized(random)
    case _ => BaseSwap.deterministic
  }

  implicit val baseChange: BaseChange =
    if (baseChangeRecomputes)
      BaseChange.fromScratch
    else if (baseChangeConjugates)
      BaseChange.swapConjugation
    else
      BaseChange.swap

  implicit def permGrpChainBuilder[G:ClassTag](implicit permutation: Permutation[G]): PermGrpChainBuilder[G, permutation.type] =
    new PermGrpChainBuilder[G, permutation.type]()(implicitly, implicitly, implicitly, permutation, implicitly)

  implicit def wrapGrpBuilder[G:ClassTag:Eq:Group:FaithfulPermRepBuilder]
    (implicit np: NoImplicit[Permutation[G]]): GrpChainBuilder[G] = new GrpChainBuilder[G]

}
