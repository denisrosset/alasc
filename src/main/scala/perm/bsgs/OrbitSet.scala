package com.faacets
package perm
package bsgs

import scala.language.higherKinds

case class OrbitSet(val beta: Dom, val orbit: Set[Dom]) extends Orbit[OrbitSet] {
  def builder = OrbitSet
  def isDefinedAt(k: Dom) = orbit(k)
  def size = orbit.size
  def updated(newGen: Iterable[PermElementLike], gens: Iterable[PermElementLike]): OrbitSet = {
    val newPoints = Set.empty[Dom] ++ (
      for (k <- orbit; g <- newGen; img = g.image(k) if !orbit.contains(img)) yield img
    )
    if (newPoints.isEmpty)
      return this
    var newOrbit = orbit ++ newPoints
    def checkForNew: Boolean = {
      val ret = false
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

object OrbitSet extends OrbitBuilder[OrbitSet] {
  def empty(beta: Dom) = OrbitSet(beta, Set(beta))
  def fromSet(beta: Dom, set: Iterable[PermElementLike]) = empty(beta).updated(set, set)
}
