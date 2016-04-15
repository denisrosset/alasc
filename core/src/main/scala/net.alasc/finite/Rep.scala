package net.alasc.finite

trait Rep[G] {

  /** Tests whether this representation can represent the element `g`. */
  def represents(g: G): Boolean

}
