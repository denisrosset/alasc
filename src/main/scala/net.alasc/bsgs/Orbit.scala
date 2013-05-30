package net.alasc
package bsgs

import scala.language.higherKinds

trait OrbitLike {
  def builder: OrbitBuilderLike
  def size: Int
  def beta: Dom
  def contains(k: Dom) = isDefinedAt(k)
  def isDefinedAt(k: Dom): Boolean
  def updated(newGen: Iterable[PermElementLike], gens: Iterable[PermElementLike]): OrbitLike
}

trait Orbit[O <: Orbit[O]] extends OrbitLike {
  def builder: OrbitBuilder[O]
  /** Add the new generators newGen to the orbit.
    * 
    * @param newGen   New generators to add to the orbit.
    * @param gens     Generators of the orbit, including newGen.
    * 
    * @return New Orbit including newGen.
    */
  def updated(newGen: Iterable[PermElementLike], gens: Iterable[PermElementLike]): O
}

trait OrbitBuilderLike {
  def empty(beta: Dom): OrbitLike
  def fromSet(beta: Dom, set: Iterable[PermElementLike]): OrbitLike
}

trait OrbitBuilder[O <: Orbit[O]] extends OrbitBuilderLike {
  def empty(beta: Dom): O
  def fromSet(beta: Dom, set: Iterable[PermElementLike]): O
}
