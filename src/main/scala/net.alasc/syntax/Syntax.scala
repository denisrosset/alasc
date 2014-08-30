package net.alasc.syntax

import net.alasc.algebra._
import scala.language.implicitConversions

trait LatticeSyntax {
  implicit def latticeSyntax[A: Lattice](a: A) = new LatticeOps(a)
}

trait CheckSyntax {
  implicit def checkSyntax[A: Check](a: A) = new CheckOps(a)
}

trait SequenceSyntax {
  implicit def sequenceSyntax[T, A](t: T)(implicit ev: Sequence[T, A]) = new SequenceOps(t)
}

trait FiniteGroupSyntax {
  implicit def finiteGroupSyntax[A: FiniteGroup](a: A) = new FiniteGroupOps(a)
}

trait PermutationActionSyntax extends FiniteGroupSyntax {
  implicit def permutationActionSyntax[A: FaithfulPermutationAction](a: A) = new PermutationActionOps(a)
}

trait ShiftablePermutationSyntax extends FiniteGroupSyntax {
  implicit def shiftablePermutationSyntax[A: ShiftablePermutation](a: A) = new ShiftablePermutationOps(a)
}

trait SubgroupSyntax {
  implicit def subgroupSyntax[S, G](s: S)(implicit ev: Subgroup[S, G]) = new SubgroupOps(s)
}

trait PermutationSubgroupSyntax {
  implicit def permutationSubgroupSyntax[S, G](s: S)(implicit ev: Subgroup[S, G], action: FaithfulPermutationAction[G]) =
    new PermutationSubgroupOps[S, G](s)
}

trait AllSyntax
    extends LatticeSyntax
    with CheckSyntax
    with SequenceSyntax
    with FiniteGroupSyntax
    with PermutationActionSyntax
    with ShiftablePermutationSyntax
    with SubgroupSyntax
    with PermutationSubgroupSyntax
