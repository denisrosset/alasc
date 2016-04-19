package net.alasc.perms

import net.alasc.algebra.{BigIndexedSeq, Permutation}
import net.alasc.domains.Partition
import net.alasc.finite.{Grp, GrpBuilder}
import spire.util.Opt

import net.alasc.bsgs.Transversal

abstract class PermGrpBuilder[G] extends GrpBuilder[G] {

  type GG <: PermGrp[G]

  implicit def permutation: Permutation[G]

  def group = permutation

  def equ = permutation

  def setwiseStabilizer(grp: Grp[G], set: Set[Int]): GG

  def pointwiseStabilizer(grp: Grp[G], set: Set[Int]): GG

  def stabilizerTransversal(grp: Grp[G], b: Int): (GG, Transversal[G])

  def someStabilizerTransversal(grp: Grp[G]): Opt[(GG, Transversal[G])]

  def stabilizer(grp: Grp[G], b: Int): GG

  def someStabilizer(grp: Grp[G]): Opt[GG]

  def fixingPartition(grp: Grp[G], partition: Partition): GG

  def subgroupFor(grp: Grp[G], backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): GG

  def lexElements(grp: Grp[G]): BigIndexedSeq[G]

  def base(grp: Grp[G]): Seq[Int]

  def find[Q:Permutation](grp: Grp[G], q: Q): Opt[G]

}

object PermGrpBuilder {

  type Aux[G, GG0 <: PermGrp[G]] = PermGrpBuilder[G] { type GG = GG0 }

}