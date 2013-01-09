package com.faacets.perm

package object Implicits {
  type Domain = Int
  type Base = Seq[Domain]

  implicit def empowerMyDomain(alpha: Domain) = new EmpoweredDomain(alpha)
  implicit def Cycle2Permutation(value: Cycle) = value.toPermutation(value.size)

  implicit def permutationOrdering: Ordering[Permutation] = {
    import scala.math.Ordering.Implicits._
    Ordering.fromLessThan(_.images < _.images)
  }
}
