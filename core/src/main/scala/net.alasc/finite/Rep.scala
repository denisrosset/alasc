package net.alasc.finite

trait Rep[G] { self =>

  /** Tests whether this representation can represent the element `g`. */
  def represents(g: G): Boolean

  /** Wraps the given group element, to be represented by this representation. */
  def apply(g: G): Reped[G, self.type] = {
    require(represents(g))
    new Reped[G, self.type](g)
  }

}
