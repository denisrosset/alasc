package com.faacets.perm

trait ActionGroup extends PermutationGroup {
  import scala.util.Random

  type Group = ActionGroup
  type Element = Action

  type RepresentedGroup <: FiniteGroup
  type RepresentedElement = representedGroup.Element

  val representedGroup: RepresentedGroup

  def imageOfElement(g: RepresentedElement, e: Domain): Domain
  def imagesOfElement(g: RepresentedElement): Array[Domain]

  def generatorsIterator = representedGroup.generatorsIterator.map(Action(_))
  def elementsIterator = representedGroup.elementsIterator.map(Action(_))
  def contains(a: Element) = representedGroup.contains(a.g)
  def identity = Action(representedGroup.identity)
  def randomElement()(implicit gen: Random = Random) = Action(representedGroup.randomElement)
  def order = representedGroup.order

  def assertValid = representedGroup.assertValid

  case class Action(g: RepresentedElement) extends PermutationElement {
    self: Element =>
    val group = ActionGroup.this
    def image(e: Domain) = imageOfElement(g, e)
    def images = imagesOfElement(g)
    override def toString: String = this.getClass.getName + "(" + g.toString + ")"
    def assertValid { g.assertValid }
    def isIdentity = g.isIdentity
    def *(that: Element) = Action(g * that.g)
    override def compare(that: Element): Int = {
      val firstNotEqual = (0 until domainSize).find(i => image(i) != that.image(i))
      firstNotEqual match {
        case None => 0
        // return -1 iff this < that
        case Some(i) if image(i) < that.image(i) => -1
        case _ => 1
      }
    }
    def inverse: Element = Action(g.inverse)
    def equal(that: Element) = (g == that.g)
   }
}
