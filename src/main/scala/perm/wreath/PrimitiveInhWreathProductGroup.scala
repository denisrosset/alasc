package com.faacets.perm
package wreath

import com.faacets.math._

case class PrimitiveInhWreathProductGroup[R <: InhWreathProductGroup](override val r: R)(implicit evidence: R#A <:< PermutationGroup) extends ActionFiniteGroup {
  override type Group = PrimitiveInhWreathProductGroup[R]
  override type Element = PrimitiveInhWreathProductAction

  type RepresentedGroup = R

  def make(h: RepresentedElement) = PrimitiveInhWreathProductAction(h)

  def degree = r.avec.map(_.degree).product
  def assertValid = r.assertValid

  case class PrimitiveInhWreathProductAction(override val g: RepresentedElement) extends Action {
    self: Element =>
    val group = PrimitiveInhWreathProductGroup.this
    def image(el: Domain) = {
      val gaelvec = g.aelvec.asInstanceOf[Vector[PermutationGroup#PermutationElement]]
      val dims = gaelvec.map(_.domainSize)
      val alpha = ind2sub(dims, el)
      val alpha1 = alpha.zipWithIndex.map { case (a, i) => {
        val i1 = g.hel.inverse.image(i)
        gaelvec(i).image(alpha(i1))
      } }
      sub2ind(dims, alpha1)
    }
    lazy val images = {
      val gaelvec = g.aelvec.asInstanceOf[Vector[PermutationGroup#PermutationElement]]
      val dims = gaelvec.map(_.domainSize)
      val P = scala.collection.mutable.ArrayBuffer.fill[Int](domainSize)(0)
      for (ind <- 0 until domainSize) {
        val alpha = ind2sub(dims, ind)
        val alpha1 = alpha.zipWithIndex.map { case (a, i) => {
          val i1 = g.hel.inverse.image(i)
          gaelvec(i).image(alpha(i1))
        } }
        P(ind) = sub2ind(dims, alpha1)
      }
      P.toArray
    }
  }
}
