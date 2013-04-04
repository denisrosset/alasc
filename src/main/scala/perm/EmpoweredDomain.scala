package com.faacets.perm

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
class EmpoweredDomain(alpha: Domain) {
  def **[P <: Permutation[P]](perm: P) = perm.image(alpha)
}
