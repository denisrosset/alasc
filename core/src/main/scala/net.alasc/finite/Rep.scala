package net.alasc.finite

import net.alasc.algebra.PermutationAction

trait Rep[G] {

  /** Tests whether this representation can represent the element `g`. */
  def represents(g: G): Boolean

}
