package com.faacets.perm
package wreath

import com.faacets.math._
trait WreathAction extends ActionGroup {
  type RepresentedGroup = WreathProductGroup
  type BottomElement = representedGroup.k.a.Element
  def imageOfBottomElement(g: BottomElement, e: Domain): Domain
  def imagesOfBottomElement(g: BottomElement): Array[Domain]
  val dims: Vector[Int]
}

trait BottomIsPermutationGroup extends WreathAction {
  val dims = Vector.fill[Int](representedGroup.k.n)(representedGroup.k.a.asInstanceOf[PermutationGroup].degree)
  def imageOfBottomElement(g: BottomElement, e: Domain) = g.asInstanceOf[PermutationGroup#PermutationElement].image(e)
  def imagesOfBottomElement(g: BottomElement) = g.asInstanceOf[PermutationGroup#PermutationElement].images
}
