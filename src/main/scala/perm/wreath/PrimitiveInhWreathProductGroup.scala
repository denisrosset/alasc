package com.faacets.perm

package wreath {
  import com.faacets.math._

  trait PrimitiveInhWreathProductGroup extends InhWreathProductGroup with PermutationGroup {
    type Group <: PrimitiveInhWreathProductGroup
    type Element <: PrimitiveInhWreathProductAction

    type H <: PermutationGroup
    type A <: PermutationGroup

    def degree = avec.map(_.degree).product

    trait PrimitiveInhWreathProductAction extends InhWreathProductElement with Permutation {
      override val domainSize = aelvec.map(_.domainSize).product
      lazy val images = {
        val dims = aelvec.map(_.domainSize)
        val P = scala.collection.mutable.ArrayBuffer.fill[Int](domainSize)(0)
        for (ind <- 0 until domainSize) {
          val alpha = ind2sub(dims, ind)
          val alpha1 = alpha.view.zipWithIndex.map { case (a, i) => {
            val i1 = hel.inverse.image(i)
            aelvec(i).image(alpha(i1))
          } }
          P(ind) = sub2ind(dims, alpha1)
        }
        P.toVector
      }
    }
  }
}
