package com.faacets.perm

package wreath {
  import com.faacets.math._

  trait ImprimitiveInhWreathProductGroup extends InhWreathProductGroup with PermutationGroup {
    type Group <: ImprimitiveInhWreathProductGroup
    type Element <: ImprimitiveInhWreathProductAction

    type H <: PermutationGroup
    type A <: PermutationGroup

    def degree = avec.map(_.degree).sum

    trait ImprimitiveInhWreathProductAction extends InhWreathProductElement with Permutation {
      lazy val images = {
        val P = scala.collection.mutable.ArrayBuffer.fill[Int](domainSize)(0)
        val sizes = aelvec.map(_.domainSize)
        val aStart = sizes.scanLeft(0)(_+_)
        for ((n, i) <- sizes.zipWithIndex) {
          for (o <- 0 until n) {
            val i1 = hel.image(i)
            val o1 = aelvec(i1).image(o)
            P(aStart(i) + o) = aStart(i1) + o1
          }
        }
        P.toVector
      }
    }
  }
}
