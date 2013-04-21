package com.faacets.perm

trait ActionFiniteGroup extends PermutationGroup {
  import scala.util.Random

  type Group <: ActionFiniteGroup
  type Element <: Action

  type RepresentedGroup <: FiniteGroup
  type RepresentedElement = r.Element

  val r: RepresentedGroup

  def make(h: RepresentedElement): Element
  def generatorsIterator = r.generatorsIterator.map(make(_))
  def elementsIterator = r.elementsIterator.map(make(_))
  def contains(a: Element) = r.contains(a.g)
  def identity = make(r.identity)
  def randomElement()(implicit gen: Random = Random) = make(r.randomElement)
  def order = r.order


  trait Action extends PermutationElement {
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
