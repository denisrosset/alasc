package com.faacets.perm

package object Implicits {
  type Domain = Int
  type Base = Seq[Domain]

  implicit def empowerMyDomain(alpha: Domain) = new EmpoweredDomain(alpha)

  implicit def permutationOrdering[P <: Permutation[P]]: Ordering[P] = {
    import scala.math.Ordering.Implicits._
    Ordering.fromLessThan(_.images < _.images)
  }
}
