package net.alasc

package object bsgs {
  type Predicate[E <: PermElement[E]] = (E => Boolean)
}
