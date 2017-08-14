package net.alasc.perms

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BaseChange, BaseSwap, GrpChainPermutationAction, SchreierSims}
import net.alasc.finite.{FaithfulActionBuilder, FaithfulPermutationActionBuilder}

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

  class PRGB[G:FaithfulPermutationActionBuilder](implicit val baseChange: BaseChange, val baseSwap: BaseSwap, val equ: Eq[G],
                                                 val classTag: ClassTag[G], val group: Group[G], val schreierSims: SchreierSims) extends GrpChainPermutationAction[G] {

    def faithfulAction(generators: Iterable[G]): PermutationAction[G] = FaithfulActionBuilder[G, Int, PermutationAction[G]].apply(generators)

  }

  implicit def permRepGrpBuilder[G:ClassTag:Eq:Group:FaithfulPermutationActionBuilder]: GrpChainPermutationAction[G] = new PRGB[G]

}
