package net.alasc.math

import net.alasc.algebra._

/** Builder for a BSGS chain.
  * 
  * Inspired by scala.collection.mutable.ListBuffer.
  */
final class BSGSBuilder[P](implicit val ev: Permutation[P]) {
  private var start: BSGS[P] = new BSGSTerm[P]
  private var last0: BSGSNode[P] = _
  private var exported: Boolean = false
  private var len = 0

  def toBSGS: BSGS[P] = {
    exported = !start.isTerminal
    start
  }
}
