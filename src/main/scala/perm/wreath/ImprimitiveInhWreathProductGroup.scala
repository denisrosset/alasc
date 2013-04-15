package com.faacets.perm

package wreath {
  import com.faacets.math._

  case class ImprimitiveInhWreathProductGroup[R <: InhWreathProductGroup](r: R) extends ActionGroup {
    type Group = ImprimitiveInhWreathProductGroup[R]
    type Element = ImprimitiveInhWreathProductAction

    type RepresentedGroup = R


    def make(h: RepresentedElement) = ImprimitiveInhWreathProductAction(h)

    def degree = r.avec.map(_.degree).sum

    def order = r.order

    def identity = make(r.identity)
    def randomElement = make(r.randomElement)
    def assertValid = r.assertValid
    def contains(a: Element) = r.contains(a.g)
    def generators = r.generators.map(make(_))
    def elements = r.elements.map(make(_))

    case class ImprimitiveInhWreathProductAction(val g: RepresentedElement) extends Action {
      self: Element =>
      val group = ImprimitiveInhWreathProductGroup.this
      def image(el: Domain) = 0
      lazy val images = {
        val P = scala.collection.mutable.ArrayBuffer.fill[Int](domainSize)(0)
        val sizes = g.aelvec.map(_.domainSize)
        val aStart = sizes.scanLeft(0)(_+_)
        for ((n, i) <- sizes.zipWithIndex) {
          for (o <- 0 until n) {
            val i1 = g.hel.image(i)
            val o1 = g.aelvec(i1).image(o)
            P(aStart(i) + o) = aStart(i1) + o1
          }
        }
        P.toVector
      }
    }
  }
}
