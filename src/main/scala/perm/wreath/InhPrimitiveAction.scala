package com.faacets
package perm
package wreath

import com.faacets.math.{ind2sub, sub2ind}

class InhPrimitiveAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: InhWreathGroup[A, AE, H, HE], WE <: InhWreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]](val ba: Array[Action[AE, Perm]], val d: Array[Int]) extends InhWreathAction[WG, WEG, W, WE, A, AE, H, HE] {
  def apply(weg: WEG) = {
    val we = weg.asInstanceOf[WE]
    val dim = d.product
    def image(k: Domain) = {
      val alpha = ind2sub(d, k.zeroBased)
      val alpha1 = new Array[Int](alpha.size)
      for (i <- 0 until alpha.size) {
        val action = ba(i)
        alpha1(we.he.image(Domain.zeroBased(i)).zeroBased) = ba(i)(we.ke(Domain.zeroBased(i))).image(Domain.zeroBased(alpha(i))).zeroBased
      }
      Domain.zeroBased(sub2ind(d, alpha1))
    }
    new Perm(Array.tabulate(dim)( k => image(Domain.zeroBased(k)).zeroBased))
  }
}
