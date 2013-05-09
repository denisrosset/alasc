package com.faacets
package perm
package bsgs

import scala.language.higherKinds

case class OrbitSet(val beta: Dom, val orbit: Set[Dom]) extends Orbit[OrbitSet] {
  def isDefinedAt(k: Dom) = orbit(k)
  def updated[E <: PermElementLike](newGen: Iterable[E], gens: Iterable[E]): OrbitSet = {
    val newPoints = Set.empty[Dom] ++ (
      for (k <- orbit; g <- newGen; img = g.image(k) if !orbit.contains(img)) yield img
    )
    if (newPoints.isEmpty)
      return this
    var newOrbit = orbit ++ newPoints
    def checkForNew: Boolean = {
      for (k <- newOrbit; g <- gens; img = g.image(k) if !newOrbit.contains(img)) {
        newOrbit += img
        return true
      }
      false
    }
    while (checkForNew) { }
    OrbitSet(beta, newOrbit)
  }
}

object OrbitSet extends OrbitCompanion[OrbitSet] {
  def empty(beta: Dom) = OrbitSet(beta, Set(beta))
}
