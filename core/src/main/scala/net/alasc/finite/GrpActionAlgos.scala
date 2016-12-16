package net.alasc.finite

import spire.algebra.{Action, Eq, Group}
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.domains.Partition
import net.alasc.perms.Perm

trait GrpActionAlgos[G, @specialized(Int) P, A <: Action[P, G]] {

  def kernel(grp: Grp[G], action: A): Grp[G]

  def stabilizer(grp: Grp[G], action: A, p: P): Grp[G]

  def stabilizerTransversal(grp: Grp[G], action: A, p: P): (Grp[G], Transversal[G])

  def someStabilizerTransversal(grp: Grp[G], action: A): Opt[(Grp[G], Transversal[G])]

  def pointwiseStabilizer(grp: Grp[G], action: A, set: Set[P]): Grp[G]

  def setwiseStabilizer(grp: Grp[G], action: A, set: Set[P]): Grp[G]

}

trait GrpPermutationActionAlgos[G, A <: PermutationAction[G]] extends GrpActionAlgos[G, Int, A] {

  def lexElements(grp: Grp[G], action: A): Opt[BigIndexedSeq[G]]

  def fixingPartition(grp: Grp[G], action: A, partition: Partition): Grp[G]

  def base(grp: Grp[G], action: A): Opt[Seq[Int]]

  def subgroupFor(grp: Grp[G], action: A, backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean): Grp[G]

  def toPerm(grp: Grp[G], action: A)(implicit builder: GrpAlgos[Perm]): Grp[Perm]

}

trait GrpFaithfulPermutationActionAlgosImpl[G, A <: PermutationAction[G]] extends GrpPermutationActionAlgos[G, A] with GrpAlgosImpl[G] {

  def toPerm(grp: Grp[G], action: A)(implicit builder: GrpAlgos[Perm]): Grp[Perm] =
    builder.fromGeneratorsAndOrder(grp.generators.map(action.toPerm), grp.order)

  def kernel(grp: Grp[G], action: A): Grp[G] = trivial

}
