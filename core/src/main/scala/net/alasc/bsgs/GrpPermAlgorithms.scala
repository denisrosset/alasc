package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.finite.{Grp, GrpGroup}
import net.alasc.perms.Perm
import net.alasc.perms.Perm.algebra
/* TODO
class GrpPermAlgorithms(implicit val baseChange: BaseChange, val baseSwap: BaseSwap, val schreierSims: SchreierSims) extends GrpChainFaithfulPermutationAction[Perm, Perm.algebra.type] {

  def equ: Eq[Perm] = Perm.algebra
  def classTag: ClassTag[Perm] = implicitly
  def group: Group[Perm] = Perm.algebra

  def faithfulAction(generators: IndexedSeq[Perm]): Perm.algebra.type = Perm.algebra

  def compatibleAction(action: PermutationAction[Perm]): Opt[<:<[action.type, Perm.algebra.type]] =
    if (action eq Perm.algebra)
      Opt(implicitly[Perm.algebra.type <:< Perm.algebra.type].asInstanceOf[<:<[action.type, Perm.algebra.type]])
  else
    Opt.empty[<:<[action.type, Perm.algebra.type]]

  def kernel(grp: Grp[Perm], action: algebra.type): Grp[Perm] = trivial

  def smallGeneratingSet(grp: Grp[Perm]): IndexedSeq[Perm] = grp.generators // TODO: implement

  def toPerm(grp: Grp[Perm], action: algebra.type)(implicit builder: GrpGroup[Perm]): Grp[Perm] = grp

}
*/