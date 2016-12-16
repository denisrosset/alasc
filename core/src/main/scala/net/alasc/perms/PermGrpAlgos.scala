package net.alasc.perms

import spire.algebra.{Eq, Group}

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.domains.Partition
import net.alasc.finite.{Grp, GrpAlgos, GrpPermutationActionAlgos}
import spire.util.Opt

import net.alasc.bsgs.Transversal

trait PermGrpAlgos extends GrpAlgos[Perm] /*with GrpPermutationActionAlgos[Perm, Perm.algebra.type]*/ {

  def group: Group[Perm] = Perm.algebra

  def equ: Eq[Perm] = Perm.algebra

  def setwiseStabilizer(grp: Grp[Perm], set: Set[Int]): Grp[Perm]

  def pointwiseStabilizer(grp: Grp[Perm], set: Set[Int]): Grp[Perm]

  def stabilizerTransversal(grp: Grp[Perm], b: Int): (Grp[Perm], Transversal[Perm, Perm.algebra.type])

  def someStabilizerTransversal(grp: Grp[Perm]): Opt[(Grp[Perm], Transversal[Perm, Perm.algebra.type])]

  def stabilizer(grp: Grp[Perm], b: Int): Grp[Perm]

  def fixingPartition(grp: Grp[Perm], partition: Partition): Grp[Perm]

  def subgroupFor(grp: Grp[Perm], backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean): Grp[Perm]

  def lexElements(grp: Grp[Perm]): BigIndexedSeq[Perm]

  def base(grp: Grp[Perm]): Seq[Int]

  def find[Q:Eq:Group](grp: Grp[Perm], actionQ: PermutationAction[Q], q: Q): Opt[Perm]

}
