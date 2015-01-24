package net.alasc.algebra

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

import spire.algebra.PartialOrder
import spire.algebra.lattice.{BoundedJoinSemilattice, Lattice}
import spire.syntax.lattice._
import spire.util.Nullbox

import net.alasc.syntax.permutationAction._

/** Faithful permutation representation of a finite group on the domain 0 ... n - 1, where
  * n is the size of the representation.
  * 
  * Representation differs from `FaithfulPermutationAction` by having a defined size, and can be
  * by construction correct only a subset of elements of type G. The function `represents` can be
  * used to check the validity of the representation of a given element. The usage of `action` on
  * invalid elements produces undefined results.
  */
trait Representation[G] {
  self =>
  /** Size of the representation, constraining the support of any permutation in 0 ... n-1. */
  def size: Int
  /** Faithful permutation action used to represent the finite group. */
  def action: FaithfulPermutationAction[G]
  def images(g: G): IndexedSeq[Int] = action.images(g, size)
  /** Tests if this representation can represent the element `g`. */
  def represents(g: G): Boolean
  val representations: Representations[G]
}
