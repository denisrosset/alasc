package com.faacets.perm

package wreath {
  import com.faacets.math._

  case class PrimitiveInhWreathProductGroup[R <: InhWreathProductGroup](r: R) extends ActionGroup {
    override type Group = PrimitiveInhWreathProductGroup[R]
    override type Element = PrimitiveInhWreathProductAction

    type RepresentedGroup = R

    def make(h: RepresentedElement) = PrimitiveInhWreathProductAction(h)

    def degree = r.avec.map(_.degree).product

    def order = r.order

    def identity = make(r.identity)
    def randomElement = make(r.randomElement)
    def assertValid = r.assertValid
    def contains(a: Element) = r.contains(a.g)
    def generators = r.generators.map(make(_))
    def elements = r.elements.map(make(_))

    case class PrimitiveInhWreathProductAction(val g: RepresentedElement) extends Action {
      self: Element =>
      val group = PrimitiveInhWreathProductGroup.this
      def image(el: Domain) = 0
      lazy val images = {
        val dims = g.aelvec.map(_.domainSize)
        val P = scala.collection.mutable.ArrayBuffer.fill[Int](domainSize)(0)
        for (ind <- 0 until domainSize) {
          val alpha = ind2sub(dims, ind)
          val alpha1 = alpha.view.zipWithIndex.map { case (a, i) => {
            val i1 = g.hel.inverse.image(i)
            g.aelvec(i).image(alpha(i1))
          } }
          P(ind) = sub2ind(dims, alpha1)
        }
        P.toVector
      }
    }
  }
}
