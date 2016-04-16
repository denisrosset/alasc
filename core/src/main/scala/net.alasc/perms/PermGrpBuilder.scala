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

  type GG = PermGrp[G]

  def setwiseStabilizer(set: Set[Int]): Grp[G] = ???

  def pointwiseStabilizer(set: Set[Int]): Grp[G] = ???

  def stabilizerTransversal(lhs: PermGrp[G], b: Int): (Grp[G], Transversal[G]) = ???

  def someStabilizerTransversal(lhs: PermGrp[G]): Opt[(Grp[G], Transversal[G])] = ???

  def stabilizer(lhs: PermGrp[G], b: Int): Grp[G] = ???

  def someStabilizer(lhs: PermGrp[G]): Opt[Grp[G]] = ???

  def fixingPartition(lhs: PermGrp[G], partition: Partition): Grp[G]

}
