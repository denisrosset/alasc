package net.alasc.finite

trait Transversal[G] {

  def elements: Iterable[G]

  def elementFor(g: G): G

}
