package net.alasc.perms

import scala.reflect.ClassTag
import scala.util.Random

import spire.util.Opt

import net.alasc.algebra.Permutation
import net.alasc.perms.chain.PermGrpChainBuilder
import net.alasc.prep.bsgs.{BaseChange, BaseSwap, SchreierSims}

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

  implicit def permGrpChainBuilder[G:ClassTag:Permutation]: PermGrpChainBuilder[G] =
    new chain.PermGrpChainBuilder[G]

}
