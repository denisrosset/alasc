package com.faacets.perm

trait ActionGroup extends PermutationGroup {
  type Group <: ActionGroup
  type Element <: Action

  type RepresentedGroup <: AbstractGroup
  type RepresentedElement = r.Element

  val r: RepresentedGroup

  def make(h: RepresentedElement): Element

  trait Action extends Permutation {
    self: Element =>

    val g: RepresentedElement

    override def toString: String = this.getClass.getName + "(" + g.toString + ")"
    def assertValid { g.assertValid }
    def isIdentity = g.isIdentity

    def *(that: Element) = make(g * that.g)

    override def compare(that: Element): Int = {
      val firstNotEqual = (0 until domainSize).find(i => image(i) != that.image(i))
      firstNotEqual match {
        case None => 0
        // return -1 iff this < that
        case Some(i) if image(i) < that.image(i) => -1
        case _ => 1
      }
    }

    def inverse: Element = make(g.inverse)

    def equal(that: Element) = (g == that.g)
 
  }

}
