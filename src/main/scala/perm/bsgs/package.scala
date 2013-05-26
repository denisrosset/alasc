package com.faacets
package perm
package object bsgs {
  type Base = List[Dom]
  type Predicate[E <: PermElement[E]] = (E => Boolean)
  type BaseImageTest = ((List[Dom], Int) => Boolean)
  def trivialTest(l: List[Dom], level: Int) = true
}
