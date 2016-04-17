package net.alasc.perms

import net.alasc.algebra.Permutation
import net.alasc.domains.Partition
import net.alasc.finite.{Grp, GrpBuilder}
import net.alasc.prep.bsgs.Transversal
import spire.util.Opt

abstract class PermGrpBuilder[G] extends GrpBuilder[G] {

  implicit def permutation: Permutation[G]

  def group = permutation

  def equ = permutation

  type GG <: PermGrp[G]

  def setwiseStabilizer(grp: Grp[G], set: Set[Int]): GG

  def pointwiseStabilizer(grp: Grp[G], set: Set[Int]): GG

  def stabilizerTransversal(grp: Grp[G], b: Int): (GG, Transversal[G])

  def someStabilizerTransversal(grp: Grp[G]): Opt[(GG, Transversal[G])]

  def stabilizer(grp: Grp[G], b: Int): GG

  def someStabilizer(grp: Grp[G]): Opt[GG]

  def fixingPartition(grp: Grp[G], partition: Partition): GG

  def subgroupFor(grp: Grp[G], backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): GG

}
