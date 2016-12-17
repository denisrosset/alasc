package net.alasc.perms

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.bsgs._
import net.alasc.domains.Partition
import net.alasc.finite.{Grp, GrpAlgosImpl, LeftCosets, RightCosets}
import net.alasc.perms.Perm.algebra

class PermGrpChainAlgos(implicit val baseChange: BaseChange, val baseSwap: BaseSwap, val schreierSims: SchreierSims) extends GrpChainFaithfulPermutationAction[Perm, Perm.algebra.type] {
  def equ: Eq[Perm] = Perm.algebra
  def classTag: ClassTag[Perm] = implicitly
  def group: Group[Perm] = Perm.algebra

  def actionForGenerators(generators: IndexedSeq[Perm]): Perm.algebra.type = Perm.algebra

  def compatibleAction(action: PermutationAction[Perm]): Opt[<:<[action.type, Perm.algebra.type]] =
    if (action eq Perm.algebra)
      Opt(implicitly[Perm.algebra.type <:< Perm.algebra.type].asInstanceOf[<:<[action.type, Perm.algebra.type]])
  else
    Opt.empty[<:<[action.type, Perm.algebra.type]]

}
