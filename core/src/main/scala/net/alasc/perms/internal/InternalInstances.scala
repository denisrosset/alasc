package net.alasc.perms.internal

import spire.algebra.{Eq, Group}
import net.alasc.algebra.PermutationAction

trait InternalInstances {

  implicit def mutPrmEq[I <: XInt]: Eq[MutPrm[I]] = GenPrm.equ.asInstanceOf[Eq[MutPrm[I]]]

  implicit def prmEq: Eq[Prm] = GenPrm.equ.asInstanceOf[Eq[Prm]]

  implicit val prmGroup: Group[Prm] = new PrmGroup

  implicit def mutPrmPermutationAction[I <: XInt]: PermutationAction[MutPrm[I]] =
    GenPrm.permutationAction.asInstanceOf[PermutationAction[MutPrm[I]]]

  implicit def prmPermutationAction: PermutationAction[Prm] =
    GenPrm.permutationAction.asInstanceOf[PermutationAction[Prm]]

}
