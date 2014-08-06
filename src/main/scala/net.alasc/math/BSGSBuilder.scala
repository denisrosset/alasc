package net.alasc.math

import net.alasc.algebra._

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
