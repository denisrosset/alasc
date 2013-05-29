package com.faacets
package perm
package object bsgs {
  type Base = List[Dom]
  type Predicate[E <: PermElement[E]] = (E => Boolean)
}
