package net.alasc.prep

import scala.reflect.ClassTag

import net.alasc.algebra._
import net.alasc.finite.{BuiltRep, Rep, RepBuilder}
import net.alasc.perms.Perm
import net.alasc.syntax.permutationAction._

trait PRep[G] extends Rep[G] {

  implicit def permutationAction: PermutationAction[G]

  /** Size of the representation, constraining the support of any permutation in 0 ... n-1. */
  def size: Int

}

trait FaithfulPRep[G] extends PRep[G] {

  implicit def permutationAction: FaithfulPermutationAction[G]

}

abstract class PRepBuilder[G] extends RepBuilder[G] {

  type R <: FaithfulPRep[G] with BuiltRep.In[this.type, G]

}
