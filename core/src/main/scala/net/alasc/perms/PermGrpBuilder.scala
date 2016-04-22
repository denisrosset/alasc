package net.alasc.perms

import spire.algebra.{Eq, Group}

import net.alasc.algebra.{BigIndexedSeq, Permutation, PermutationAction}
import net.alasc.domains.Partition
import net.alasc.finite.{Grp, GrpBuilder}
import spire.util.Opt

import net.alasc.bsgs.Transversal

abstract class PermGrpBuilder[G] extends GrpBuilder[G] {

  implicit def permutation: Permutation[G]

  def group: Group[G] = permutation

  def equ: Eq[G] = permutation

  def setwiseStabilizer(grp: Grp[G], set: Set[Int]): GG

  def pointwiseStabilizer(grp: Grp[G], set: Set[Int]): GG

  def stabilizerTransversal(grp: Grp[G], b: Int): (GG, Transversal[G, _ <: PermutationAction[G] with Singleton])

  def someStabilizerTransversal(grp: Grp[G]): Opt[(GG, Transversal[G, _ <: PermutationAction[G] with Singleton])]

  def stabilizer(grp: Grp[G], b: Int): GG

  def fixingPartition(grp: Grp[G], partition: Partition): GG

  def subgroupFor(grp: Grp[G], backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): GG

  def lexElements(grp: Grp[G]): BigIndexedSeq[G]

  def base(grp: Grp[G]): Seq[Int]

  def find[Q:Permutation](grp: Grp[G], q: Q): Opt[G]

}
