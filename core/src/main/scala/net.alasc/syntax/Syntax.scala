package net.alasc.syntax

import spire.algebra.Monoid
import spire.syntax.GroupSyntax
import net.alasc.algebra._
import scala.language.implicitConversions

trait CheckSyntax {
  implicit def checkSyntax[A: Check](a: A) = new CheckOps(a)
}

trait PermutationActionSyntax {
  implicit def permutationActionSyntax[A: PermutationAction](a: A) = new PermutationActionOps(a)
}

trait ShiftablePermutationSyntax extends PermutationActionSyntax {
  implicit def shiftablePermutationSyntax[A: ShiftablePermutation](a: A) = new ShiftablePermutationOps(a)
}

trait WithBaseSyntax {
  implicit def withBaseOps[G, B](g: G)(implicit ev: WithBase[G, B]) = new WithBaseSemigroupoidOps[G, B](g)
}

trait PartialMonoidWithBaseSyntax {
  implicit def partialMonoidWithBaseOps[G, B](b: B)(implicit ev: PartialMonoidWithBase[G, B]) = new PartialMonoidWithBaseOps[G, B](b)
}

trait AllSyntax
    extends CheckSyntax
    with PermutationActionSyntax
    with ShiftablePermutationSyntax
    with WithBaseSyntax
    with PartialMonoidWithBaseSyntax
