package net.alasc.syntax

import spire.algebra.Monoid
import net.alasc.algebra._
import scala.language.implicitConversions

trait CheckSyntax {
  implicit def checkSyntax[A: Check](a: A) = new CheckOps(a)
}

trait MonoidSyntax {
  implicit def monoidSyntax[A: Monoid](as: TraversableOnce[A]) = new MonoidOps(as)
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
  implicit def permutationSubgroupSyntax[S, G](s: S)(implicit ev: Subgroup[S, G], action: FaithfulPermutationAction[G]) =
    new PermutationSubgroupOps[S, G](s)
}

trait WithBaseSyntax {
  implicit def withBaseOps[G, B](g: G)(implicit ev: WithBase[G, B]) = new WithBaseSemigroupoidOps[G, B](g)
}

trait PartialMonoidWithBaseSyntax {
  implicit def partialMonoidWithBaseOps[G, B](b: B)(implicit ev: PartialMonoidWithBase[G, B]) = new PartialMonoidWithBaseOps[G, B](b)
}


trait AllSyntax
    extends CheckSyntax
    with MonoidSyntax
    with FiniteGroupSyntax
    with PermutationActionSyntax
    with ShiftablePermutationSyntax
    with SubgroupSyntax
    with PermutationSubgroupSyntax
    with WithBaseSyntax
    with PartialMonoidWithBaseSyntax
