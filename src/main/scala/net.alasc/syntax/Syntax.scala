package net.alasc.syntax

import net.alasc.algebra._
import scala.language.implicitConversions

trait CheckSyntax {
  implicit def checkSyntax[A: Check](a: A) = new CheckOps(a)
}

trait LengthSyntax {
  implicit def lengthSyntax[A: Length](a: A) = new LengthOps(a)  
}

trait BigLengthSyntax {
  implicit def bigLengthSyntax[A: BigLength](a: A) = new BigLengthOps(a)  
}

trait IndexSyntax extends LengthSyntax {
  implicit def indexSyntax[T, A](t: T)(implicit ev: Index[T, A]) = new IndexOps(t)
}

trait BigIndexSyntax extends BigLengthSyntax {
  implicit def bigIndexSyntax[T, A](t: T)(implicit ev: BigIndex[T, A]) = new BigIndexOps(t)
}

trait FiniteGroupSyntax {
  implicit def finiteGroupSyntax[A: FiniteGroup](a: A) = new FiniteGroupOps(a)
}

trait PermutationActionSyntax extends FiniteGroupSyntax {
  implicit def permutationActionSyntax[A: PermutationAction](a: A) = new PermutationActionOps(a)
}

trait ShiftablePermutationSyntax extends FiniteGroupSyntax {
  implicit def shiftablePermutationSyntax[A: ShiftablePermutation](a: A) = new ShiftablePermutationOps(a)
}

trait SubgroupSyntax {
  implicit def subgroupSyntax[S, G](s: S)(implicit ev: Subgroup[S, G]) = new SubgroupOps(s)
}

trait PermutationSubgroupSyntax {
  implicit def permutationSubgroupSyntax[S, G](s: S)(implicit ev: Subgroup[S, G], action: PermutationAction[G]) =
    new PermutationSubgroupOps[S, G](s)
}

trait AllSyntax
    extends CheckSyntax
    with LengthSyntax
    with BigLengthSyntax
    with IndexSyntax
    with BigIndexSyntax
    with FiniteGroupSyntax
    with PermutationActionSyntax
    with ShiftablePermutationSyntax
    with SubgroupSyntax
    with PermutationSubgroupSyntax
