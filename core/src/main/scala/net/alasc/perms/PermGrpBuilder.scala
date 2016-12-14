package net.alasc.perms

import spire.algebra.{Eq, Group}

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.domains.Partition
import net.alasc.finite.{Grp, GrpBuilder}
import spire.util.Opt

import net.alasc.bsgs.Transversal

trait PermGrpBuilder extends GrpBuilder[Perm] {

  def group: Group[Perm] = Perm.algebra

  def equ: Eq[Perm] = Perm.algebra

  def setwiseStabilizer(grp: Grp[Perm], set: Set[Int]): GG

  def pointwiseStabilizer(grp: Grp[Perm], set: Set[Int]): GG

  def stabilizerTransversal(grp: Grp[Perm], b: Int): (GG, Transversal[Perm, Perm.algebra.type])

  def someStabilizerTransversal(grp: Grp[Perm]): Opt[(GG, Transversal[Perm, Perm.algebra.type])]

  def stabilizer(grp: Grp[Perm], b: Int): GG

  def fixingPartition(grp: Grp[Perm], partition: Partition): GG

  def subgroupFor(grp: Grp[Perm], backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean): GG

  def lexElements(grp: Grp[Perm]): BigIndexedSeq[Perm]

  def base(grp: Grp[Perm]): Seq[Int]

  def find[Q:Eq:Group](grp: Grp[Perm], actionQ: PermutationAction[Q], q: Q): Opt[Perm]

}
