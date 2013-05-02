package com.faacets
package perm
package wreath

import com.faacets.math.{ind2sub, sub2ind}

class PrimitiveAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: WreathGroup[A, AE, H, HE], WE <: WreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]](val ba: Action[AE, Perm], val d: Int) extends WreathAction[WG, WEG, W, WE, A, AE, H, HE] {
  def apply(weg: WEG) = {
    val we = weg.asInstanceOf[WE]
    val dims = Array.fill[Int](we.he.size)(d)
    val dim = dims.product
    def image(k: Domain) = {
      val alpha = ind2sub(dims, k.zeroBased)
      val alpha1 = new Array[Int](alpha.size)
      for (i <- 0 until alpha.size) {
        alpha1(we.he.image(Domain.zeroBased(i)).zeroBased) = ba(we.ke(Domain.zeroBased(i))).image(Domain.zeroBased(alpha(i))).zeroBased
      }
      Domain.zeroBased(sub2ind(dims, alpha1))
    }
    new Perm(Array.tabulate(dim)( k => image(Domain.zeroBased(k)).zeroBased ))
  }
}
