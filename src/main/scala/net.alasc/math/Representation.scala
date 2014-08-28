package net.alasc.algebra

import net.alasc.syntax.joinSemilatticeSyntax._

/** Faithful representation of a finite group on the domain 0 ... n - 1, where
  * n is the size of the representation. */
trait Representation[G] extends PermutationAction[G] {
  /** Size of the representation. */
  def size: Int
}

/** Describes a family of permutation representations of a group G. Depending on the particular subgroup H of G,
  * the permutation representation can differ: thus, one obtains a representation valid for a set of generators
  * by calling `representation`.
  *
  * Given representations a and b, one defines a joined representation c such that if A and B are subgroups
  * of G for which a and b are valid, the representation c is valid for the union of A and B. This is described
  * using a join-semilattice.
  */ 
trait Representations[R <: Representation[G], G] {
  implicit def lattice: JoinSemilattice[R]
  def canBeCast(r: Representation[G]): Boolean
  def cast(r: Representation[G]): R
  def representation(generators: Iterable[G]): R
  def genericJoin(r1: Representation[G], r2: Representation[G],
    generators1: Iterable[G], generators2: Iterable[G]): R =
    if (canBeCast(r1)) {
      if (canBeCast(r2))
        cast(r1) join cast(r2)
      else
        cast(r1) join representation(generators2)
    } else {
      if (canBeCast(r2))
        representation(generators1) join cast(r2)
      else
        representation(generators1 ++ generators2)
    }
}

case class UniqueRepresentation[G](uniqueR: Representation[G]) extends Representations[Representation[G], G] {
  implicit val lattice = new JoinSemilattice[Representation[G]] {
    def partialCompare(x: Representation[G], y: Representation[G]) =
      if (x == y) 0.0 else Double.NaN
    def join(x: Representation[G], y: Representation[G]) = {
      require(x == uniqueR)
      require(y == uniqueR)
      uniqueR
    }
  }
  def representation(generators: Iterable[G]) = uniqueR
  def cast(r: Representation[G]) = {
    require(r == uniqueR)
    r
  }
  def canBeCast(r: Representation[G]) = (r == uniqueR)
}
