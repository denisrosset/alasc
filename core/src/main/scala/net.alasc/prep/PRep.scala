package net.alasc.prep

import scala.reflect.{classTag, ClassTag}

import spire.algebra.{Group, PartialOrder}
import spire.algebra.lattice.{BoundedJoinSemilattice, Lattice}

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

final class UniquePRepBuilder[G](val size: Int)(implicit val action: FaithfulPermutationAction[G]) extends PRepBuilder[G] {

  type R = UniquePRep.type

  def classTagR = classTag[UniquePRep.type]

  def build(generators: Iterable[G]) = UniquePRep

  object UniquePRep extends BuiltRep[G] with FaithfulPRep[G] {
    type B = UniquePRepBuilder.this.type
    val builder: B = UniquePRepBuilder.this
    def permutationAction = builder.action
    def represents(g: G) = true
    def size = builder.size
  }

  object lattice extends Lattice[R] with BoundedJoinSemilattice[R] {
    def zero = UniquePRep
    def join(lhs: R, rhs: R) = UniquePRep
    def meet(lhs: R, rhs: R) = UniquePRep
  }

  object partialOrder extends PartialOrder[R] {
    def partialCompare(lhs: R, rhs: R): Double = 0.0
  }

}
