package com.faacets.perm

package wreath {
  import com.faacets.math._

  case class PrimitiveInhWreathProductAction[HE <: Permutation[HE], AE <: Permutation[AE]](
    override val g: InhWreathProductElement[HE, AE]) extends Action[InhWreathProductElement[HE, AE], PrimitiveInhWreathProductAction[HE, AE]] {
    type GE = InhWreathProductElement[HE, AE]
    val domainSize = g.aelvec.map(_.domainSize).product
    def build(g1: GE) = new PrimitiveInhWreathProductAction(g1)
    override def images = _images
    lazy val _images = {
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
      Vector(P:_*)
    }
  }
}
