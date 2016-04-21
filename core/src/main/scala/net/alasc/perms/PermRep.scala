package net.alasc.perms

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Rep

trait PermRep[G] extends Rep[G] {

  implicit def permutationAction: PermutationAction[G]

  /** Size of the representation, constraining the support of any permutation in 0 ... n-1. */
  def size: Int

}

trait FaithfulPermRep[G] extends PermRep[G] {

  type F <: PermutationAction[G] with Singleton

  implicit def permutationAction: F

}
