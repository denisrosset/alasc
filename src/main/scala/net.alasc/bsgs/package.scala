package net.alasc

package object bsgs {
  type Base = List[Dom]
  type Predicate[E <: PermElement[E]] = (E => Boolean)
}
