package com.faacets.perm

package object bsgs {
  implicit def emptyExplicitTransversal[P <: Permutation[P]](beta: Domain, id: P): ExplicitTransversal[P] = ExplicitTransversal(beta, id)
}
