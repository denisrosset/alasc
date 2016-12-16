package net.alasc.finite

import spire.NoImplicit
import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.bsgs
import net.alasc.domains.Partition
import net.alasc.perms.{Perm, PermGrpAlgos}

trait PermutationActionGrpAlgos[G] extends GrpAlgos[G] {

  type GG <: Grp[G]

  def kernel(grp: Grp[G], action: PermutationAction[G]): GG

  def lexElements(grp: Grp[G], action: PermutationAction[G]): Opt[BigIndexedSeq[G]]

  def fixingPartition(grp: Grp[G], action: PermutationAction[G], partition: Partition): GG

  def stabilizer(grp: Grp[G], action: PermutationAction[G], b: Int): GG

  def someStabilizerTransversal(grp: Grp[G], action: PermutationAction[G]): Opt[(GG, bsgs.Transversal[G, action.type])]

  def stabilizerTransversal(grp: Grp[G], action: PermutationAction[G], b: Int): (GG, bsgs.Transversal[G, action.type])

  def pointwiseStabilizer(grp: Grp[G], action: PermutationAction[G], set: Set[Int]): GG

  def setwiseStabilizer(grp: Grp[G], action: PermutationAction[G], set: Set[Int]): GG

  def find[Q:Eq:Group](grp: Grp[G], actionG: PermutationAction[G], actionQ: PermutationAction[Q], q: Q): Opt[G]

  def find(grp: Grp[G], actionG: PermutationAction[G], p: Perm): Opt[G]

  def subgroupFor(grp: Grp[G], action: PermutationAction[G], backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean): GG

  def base(grp: Grp[G], action: PermutationAction[G]): Opt[Seq[Int]]

  def toPerm(grp: Grp[G], action: PermutationAction[G])(implicit builder: PermGrpAlgos): Grp[Perm]
  // TODO if action is not faithful    Grp.fromGeneratorsAndOrder(generators.map(action.toPerm(_)), order)

}
