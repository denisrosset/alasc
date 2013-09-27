package net.alasc

case class OrbitSet[F <: FiniteElementLike](beta: Dom, action: Action[F], orbit: Set[Dom]) extends Orbit[F] {
  def builder = OrbitSet
  def size = orbit.size
  def isDefinedAt(k: Dom) = orbit(k)
  def updated(newGen: Iterable[F], gens: Iterable[F]): OrbitSet[F] = {
    val newPoints = Set.empty[Dom] ++ (
      for (k <- orbit; g <- newGen; img = action(g, k) if !orbit.contains(img)) yield img
    )
    if (newPoints.isEmpty)
      return this
    var newOrbit = orbit ++ newPoints
    def checkForNew: Boolean = {
      val ret = false
      for (k <- newOrbit; g <- gens; img = action(g, k) if !newOrbit.contains(img)) {
        newOrbit += img
        return true
      }
      false
    }
    while (checkForNew) { }
    OrbitSet(beta, action, newOrbit)
  }
}

object OrbitSet extends OrbitBuilder {
  def empty[F <: FiniteElementLike](beta: Dom, action: Action[F]) = 
    OrbitSet(beta, action, Set(beta))
}
