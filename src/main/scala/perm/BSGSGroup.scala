package com.faacets.perm

// WARNING: assumes through the code that the base is full !


/** A permutation group with its stabilizer chain described by a base, a strong generating set,
  * and transversals.
  * The underlying permutation type is parametrized by P, and the type of transversals
  * is parametrized by T.
  * One should probably use the Schreier-Sims construction instead of constructing the BSGSGroup
  * object directly.
  */
class BSGSGroup[P <: Permutation[P], T <: Transversal[P, T]]
  (val id: P, val S: Seq[P], val U: Vector[T]) extends PermutationGroup[P] {

  /** Create BSGSGroup from base and strong generating set, computing the transversals
    * @param id     The group identity element
    * @param sgs    Strong generating set of the group
    * @param base   Base of the group corresponding to the strong generating set
    * @param emptyU Function constructing an empty transversal from a domain element and the identity
    */
  def this(id: P, S: Seq[P], base: Seq[Domain], emptyU: (Domain, P) => T) = this(id, S, {
    // We compute the generating sets for the stabilizer chain
    val Ss = (0 to base.length).toVector.map( i => S.filter( s => base.take(i).forall( e => s.image(e) == e ) ) )
    // We compute the transversals for each level
      (0 until base.length).toVector.map( i => Ss(i).foldLeft(emptyU(base(i), id))( _+_ ) ) })

  override def toString = "BSGSGroup of order " + order + " and degree " + degree
  def identity = id
  val degree = id.domainSize
  val m = U.length /** Length of the stabilizer chain. */

  def generators = S
  def order: Int = (1 /: U)( (p:Int, u:T) => u.size*p)
  def contains(g: P): Boolean = sift(perm)._1.isIdentity

  /** Checks this BSGS construction for consistency. TODO: do an actual check. */
  def assertValid {
    assert(id.isIdentity)
    id.assertValid
  }

  /** Produces a random element */
  def randomElement = {
    var g = identity
    for (u <- U)
      g = u.randomElement * g
    g
  }

  /** Iterates through all the elements of the group, by describing any element
    * of the group as a product of members of transversals.
    */
  def iterator = {
    def iter(i: Int): Iterator[P] = {
      if (i == m) return List(identity).iterator
      if (i == m - 1) return U(i).elementsIterator
      for(g <- U(i).elementsIterator; h <- iter(i+1)) yield g*h
    }
    iter(0)
  }

  def sift(g: P, i: Int = 0): (P, Int) = {
    // we left the base? exit
    if (i >= m)
      return (g, m)
    val b = g.image(U(i).beta)
    // is the image of the current base element in the transversal ?
    if (!U(i).contains(b))
      // if not, exit
      return (g, i)
    // we fixed the current base element, on to the next one
    sift(g * U(i)(b), i + 1)
  }
}
