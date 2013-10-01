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
