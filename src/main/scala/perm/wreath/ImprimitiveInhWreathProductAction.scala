package com.faacets.perm

package wreath {
  import com.faacets.math._

  case class ImprimitiveInhWreathProductAction[HE <: Permutation[HE], AE <: Permutation[AE]](
    override val g: InhWreathProductElement[HE, AE]) extends Action[InhWreathProductElement[HE, AE], ImprimitiveInhWreathProductAction[HE, AE]] {
    type GE = InhWreathProductElement[HE, AE]
    val domainSize = g.aelvec.map(_.domainSize).sum
    def build(g1: GE) = new ImprimitiveInhWreathProductAction(g1)
    override def images = _images
    lazy val _images = {
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
      Vector(P:_*)
    }
  }
}
