package com.faacets
package perm
package bsgs

import scala.language.higherKinds

trait OrbitLike {
  def beta: Dom
  def contains(k: Dom) = isDefinedAt(k)
  def isDefinedAt(k: Dom): Boolean
}

trait Orbit[O <: Orbit[O]] extends OrbitLike {
  /** Add the new generators newGen to the orbit.
    * 
    * @param newGen   New generators to add to the orbit.
    * @param gens     Generators of the orbit, including newGen.
    * 
    * @return New Orbit including newGen.
    */
  def updated[E <: PermElementLike](newGen: Iterable[E], gens: Iterable[E]): O
}

trait OrbitCompanion[O <: Orbit[O]] {
  def empty(beta: Dom): O
  //  def fromSet[E <: PermElementLike](beta: Dom, set: Iterable[E]): O
}
