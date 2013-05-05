package com.faacets
package perm
package wreath

import com.faacets.math.{ind2sub, sub2ind}

class InhPrimitiveAction[AE <: FiniteElement[AE], HE <: PermElement[HE]](val ba: Array[Action[AE, Perm]]) extends InhWreathAction[AE, HE] {
  def apply(we: InhWreathElement[AE, HE]) = {
    val dest = (ba zip we.ke.arr).map(Function.tupled( (b, w) => b(w) ))
    val dims = dest.map(_.size)
    val dim = dims.product
    def image(k: Dom) = {
      val alpha = ind2sub(dims, k._0)
      val alpha1 = new Array[Int](alpha.size)
      for (i <- 0 until alpha.size) {
        alpha1(we.he.image(Dom._0(i))._0) = dest(i).image(Dom._0(alpha(i)))._0
      }
      Dom._0(sub2ind(dims, alpha1))
    }
    new Perm(Array.tabulate(dim)( k => image(Dom._0(k))._0))
  }
}
