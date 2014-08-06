package net.alasc.math

import net.alasc.algebra._
/** Implementation of a BSGS chain as a single linked list.
  * 
  * Inspired by scala.collection.immutable.List.
  */
sealed abstract class BSGS[P] {
  implicit def ev: Permutation[P]
  def isTerminal: Boolean
  def tail: BSGS[P]
  // def transversal
  // def strongGeneratingSet
}

final class BSGSNode[P](private[alasc] var tl: BSGS[P])(implicit val ev: Permutation[P]) extends BSGS[P] {
  def isTerminal = false
  def tail = tl
}

final class BSGSTerm[P](implicit val ev: Permutation[P]) extends BSGS[P] {
  def isTerminal = true
  def tail =
    throw new UnsupportedOperationException("tail of terminal")
}
