package net.alasc.algebra

import net.alasc.syntax.latticeSyntax._
import net.alasc.util._

/** Faithful permutation representation of a finite group on the domain 0 ... n - 1, where
  * n is the size of the representation.
  * 
  * Representation differs from `FaithfulPermutationAction` by having a defined size, and can be
  * by construction correct only a subset of elements of type G. The function `represents` can be
  * used to check the validity of the representation of a given element. The usage of `action` on
  * invalid elements produces undefined results.
  */
trait Representation[G] <: AnyRef {
  /** Size of the representation, constraining the support of any permutation in 0 ... n-1. */
  def size: Int
  /** Faithful permutation action used to represent the finite group. */
  def action: FaithfulPermutationAction[G]
  /** Tests if this representation can represent the element `g`. */
  def represents(g: G): Boolean
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
  implicit def lattice: Lattice[R]
  def tryCast(r: Representation[G]): RefOption[R]
  object Typed {
    def unapply(r: Representation[G]): RefOption[R] = tryCast(r)
  }
  def get(generators: Iterable[G]): R
  def genericJoin(r1: Representation[G], generators1: Iterable[G], generators2: Iterable[G]): R =
    r1 match {
      case Typed(rep1) => rep1 join get(generators2)
      case _ => get(generators1 ++ generators2)
    }
  def genericJoin(r1: Representation[G], r2: Representation[G],
    generators1: Iterable[G], generators2: Iterable[G]): R =
    r1 match {
      case Typed(rep1) => r2 match {
        case Typed(rep2) => rep1 join rep2
        case _ => rep1 join get(generators2)
      }
      case _ => r2 match {
        case Typed(rep2) => get(generators1) join rep2
        case _ => get(generators1 ++ generators2)
      }
    }
  def genericMeet(r1: Representation[G], r2: Representation[G],
    generators1: Iterable[G], generators2: Iterable[G]): R =
    Typed.unapply(r1).getOrElse(get(generators1)) meet Typed.unapply(r2).getOrElse(get(generators2))
}
