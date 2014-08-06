package net.alasc.syntax

import net.alasc.algebra._
import scala.language.implicitConversions

trait LengthSyntax {
  implicit def lengthSyntax[A: Length](a: A) = new LengthOps(a)  
}

trait BigLengthSyntax {
  implicit def bigLengthSyntax[A: BigLength](a: A) = new BigLengthOps(a)  
}

trait IndexSyntax extends LengthSyntax {
  implicit def indexSyntax[T, A](t: T)(implicit ev: Index[A, T]) = new IndexOps(t)
}

trait BigIndexSyntax extends BigLengthSyntax {
  implicit def bigIndexSyntax[T, A](t: T)(implicit ev: BigIndex[A, T]) = new BigIndexOps(t)
}

trait FiniteGroupSyntax {
  implicit def finiteGroupSyntax[A: FiniteGroup](a: A) = new FiniteGroupOps(a)
}

trait PermutationSyntax extends FiniteGroupSyntax {
  implicit def permutationSyntax[A: Permutation](a: A) = new PermutationOps(a)
}

trait SubgroupSyntax {
  implicit def subgroupSyntax[S, G](s: S)(implicit ev: Subgroup[S, G]) = new SubgroupOps(s)
}

trait AllSyntax
    extends LengthSyntax
    with BigLengthSyntax
    with IndexSyntax
    with BigIndexSyntax
    with FiniteGroupSyntax
    with PermutationSyntax
    with SubgroupSyntax
