package com.faacets.perm

trait Action {
  type Group <: FiniteGroup
  val group: Group
  def imageOf(gel: group.Element, el: Domain): Domain
  def imagesOf(gel: group.Element): Array[Domain]
  val dim: Int
}

class TrivialAction[G <: PermutationGroup](g: G) extends Action {
  type Group = G
  val group = g
  val dim = g.degree
  def imageOf(gel: group.Element, el: Domain) = gel.image(el)
  def imagesOf(gel: group.Element) = gel.images
}

trait ActionGroup extends PermutationGroup {
  import scala.util.Random

  type Group = ActionGroup
  type Element = ActionGroupElement

  val action: Action

  type RepresentedElement = action.group.Element

  def degree = action.dim

  def generatorsIterator =
    action.group.generatorsIterator.map(ActionGroupElement(_))
  def elementsIterator =
    action.group.elementsIterator.map(ActionGroupElement(_))
  def contains(age: Element) = action.group.contains(age.gel)
  def identity = ActionGroupElement(action.group.identity)
  def randomElement()(implicit gen: Random = Random) =
    ActionGroupElement(action.group.randomElement)
  def order = action.group.order

  def assertValid = action.group.assertValid

  case class ActionGroupElement(gel: RepresentedElement) extends PermutationElement {
    self: Element =>
    val group = ActionGroup.this
    def image(el: Domain) = action.imageOf(gel, el)
    def images = action.imagesOf(gel)
    override def toString: String = this.getClass.getName + "(" + gel.toString + ")"
    def assertValid { gel.assertValid }
    def isIdentity = gel.isIdentity
    def *(that: Element) = ActionGroupElement(gel * that.gel)
    override def compare(that: Element): Int = {
      val firstNotEqual = (0 until domainSize).find(i => image(i) != that.image(i))
      firstNotEqual match {
        case None => 0
        // return -1 iff this < that
        case Some(i) if image(i) < that.image(i) => -1
        case _ => 1
      }
    }
    def inverse: Element = ActionGroupElement(gel.inverse)
    def equal(that: Element) = (gel == that.gel)
   }
}

