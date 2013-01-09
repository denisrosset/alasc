package com.faacets.perm

import Implicits._

class EmpoweredDomain(alpha: Domain) {
  /** Permutation acting on an element. Is left-associative as required.
    * 
    * Example
    * =======
    * 
    * scala> import PermImplicits._
    * scala> val a = Cycle(0,3,4)(1,2,5)
    * scala> val b = Cycle(1,2,0,5)
    * scala> 0**a
    * res: Int = 3
    * scala> 0**b**a
    * res: Int = 1
    * scala> 0**b
    * res: Int = 5
    * scala> 5**a
    * res: Int = 1
    * 
    */
  def **(P: Permutation) = P.image(alpha)

  /** Notation for the orbit of an element. */
  //def **(G: PermutationGroup) = G.orbit(alpha)
}
