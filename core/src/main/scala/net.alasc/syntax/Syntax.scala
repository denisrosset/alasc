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

trait SequenceSyntax {
  implicit def sequenceSyntax[T, A](t: T)(implicit ev: Sequence[T, A]) = new SequenceOps(t)
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


trait SemigroupoidSyntax {
  implicit def semigroupoidOps[G <: AnyRef: Semigroupoid](g: G) = new SemigroupoidOps[G](g)
}

trait WithBaseSyntax {
  implicit def withBaseOps[G, B](g: G)(implicit ev: WithBase[G, B]) = new WithBaseSemigroupoidOps[G, B](g)
}

trait PartialMonoidSyntax extends SemigroupoidSyntax {
  implicit def partialMonoidOps[G <: AnyRef](g: G)(implicit ev: PartialMonoid[G]) = new PartialMonoidOps[G](g)
}

trait PartialMonoidWithBaseSyntax extends PartialMonoidSyntax with WithBaseSyntax {
  implicit def partialMonoidWithBaseOps[G <: AnyRef, B](b: B)(implicit ev: PartialMonoidWithBase[G, B]) = new PartialMonoidWithBaseOps[G, B](b)
}

trait GroupoidSyntax extends PartialMonoidSyntax {
  implicit def groupoidOps[G <: AnyRef](g: G)(implicit ev: Groupoid[G]) = new GroupoidOps[G](g)
}

trait PartialActionSyntax {
  implicit def partialActionGroupOps[G <: AnyRef](g: G) = new PartialActionGroupOps(g)
  implicit def partialActionPointOps[P <: AnyRef](p: P) = new PartialActionPointOps(p)
}

trait AllSyntax
    extends CheckSyntax
    with MonoidSyntax
    with SequenceSyntax
    with FiniteGroupSyntax
    with PermutationActionSyntax
    with ShiftablePermutationSyntax
    with SubgroupSyntax
    with PermutationSubgroupSyntax
    with SemigroupoidSyntax
    with WithBaseSyntax
    with PartialMonoidSyntax
    with PartialMonoidWithBaseSyntax
    with GroupoidSyntax
    with PartialActionSyntax
