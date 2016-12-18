package net.alasc.finite

import spire.algebra.Action
import spire.util.Opt

/** Group methods that depend on an Action. */
trait GrpAction[G, @specialized(Int) P, A <: Action[P, G]] {

  def kernel(grp: Grp[G], action: A): Grp[G]

  def stabilizer(grp: Grp[G], action: A, p: P): Grp[G]

  def stabilizerTransversal(grp: Grp[G], action: A, p: P): (Grp[G], Transversal[G])

  def someStabilizerTransversal(grp: Grp[G], action: A): Opt[(Grp[G], Transversal[G])]

  def pointwiseStabilizer(grp: Grp[G], action: A, set: Set[P]): Grp[G]

  def setwiseStabilizer(grp: Grp[G], action: A, set: Set[P]): Grp[G]

}
