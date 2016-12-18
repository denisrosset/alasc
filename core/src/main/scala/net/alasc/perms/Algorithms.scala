package net.alasc.perms

import scala.reflect.ClassTag
import scala.util.Random

import spire.NoImplicit
import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BaseChange, BaseSwap, GrpChainPermutationAction, GrpPermAlgorithms, SchreierSims}

abstract class Algorithms0 {

  implicit def schreierSims: SchreierSims

  implicit def baseSwap: BaseSwap

  implicit def baseChange: BaseChange

}

class Algorithms(
                  val randomOpt: Opt[Random] = Opt(Random),
                  val baseChangeRecomputes: Boolean = false,
                  val baseChangeConjugates: Boolean = true) extends Algorithms0 {

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

  implicit def permGrpChainAlgos: GrpPermAlgorithms =
    new GrpPermAlgorithms

/*
  implicit def permRepGrpBuilder[G:ClassTag:Eq:Group:FaithfulPermRepBuilder]
    (implicit np: NoImplicit[G =:= Perm]): GrpChainPermutationAction[G] = new GrpChainPermutationAction[G]
*/
}
