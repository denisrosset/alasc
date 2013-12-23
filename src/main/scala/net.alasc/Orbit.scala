/*
# Orbits #
*/
package net.alasc

trait Orbit[F <: FiniteElementLike] {
  def builder: OrbitBuilder

  def action: Action[F]
  def beta: Dom

  def size: Int
  def contains(k: Dom) = isDefinedAt(k)
  def isDefinedAt(k: Dom): Boolean

  def orbit: Set[Dom]

  /** Add the new generators newGen to the orbit.
    * 
    * @param newGen   New generators to add to the orbit.
    * @param gens     Generators of the orbit, including newGen.
    * 
    * @return New Orbit including newGen.
    */
  def updated(newGen: Iterable[F], gens: Iterable[F]): Orbit[F]
}

trait OrbitBuilder {
  def empty[F <: FiniteElementLike](beta: Dom, action: Action[F]): Orbit[F]
  def fromSet[F <: FiniteElementLike](beta: Dom, action: Action[F], set: Iterable[F]): Orbit[F] =
    empty(beta, action).updated(set, set)
}

/*
## Implementation of `Orbit` using a `Set`
*/

case class OrbitSet[F <: FiniteElementLike](beta: Dom, action: Action[F], intOrbit: collection.immutable.BitSet) extends Orbit[F] {
  def orbit = intOrbit.map(k => Dom._0(k))
  def builder = OrbitSet
  def size = intOrbit.size
  def isDefinedAt(k: Dom) = intOrbit(k._0)
  def updated(newGen: Iterable[F], gens: Iterable[F]): OrbitSet[F] = {
    val newOrbit = collection.mutable.BitSet.empty
    for (k <- intOrbit; g <- newGen)
      newOrbit += action(g, Dom._0(k))._0
    val newPoints = newOrbit.clone
    for (k <- intOrbit)
      newPoints -= k
    if (newPoints.isEmpty)
      return this
    def checkForNew: Boolean = {
      val ret = false
      for (k <- newOrbit; g <- gens) {
        val img = action(g, Dom._0(k))._0
        if (!newOrbit.contains(img)) {
          newOrbit += img
          return true
        }
      }
      false
    }
    while (checkForNew) { }
    OrbitSet(beta, action, newOrbit.toImmutable)
  }
}

object OrbitSet extends OrbitBuilder {
  def empty[F <: FiniteElementLike](beta: Dom, action: Action[F]) = 
    OrbitSet(beta, action, collection.immutable.BitSet(beta._0))
}
