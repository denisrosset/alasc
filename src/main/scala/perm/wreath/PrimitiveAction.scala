package com.faacets
package perm
package wreath

import com.faacets.math.{ind2sub, sub2ind}

class PrimitiveAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: WreathGroup[A, AE, H, HE], WE <: WreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]](val g: WG, val ba: Action[A, AE]) extends WreathAction[WG, WEG, W, WE, A, AE, H, HE] {
  val dim = dims.product
  def imageOf(weg: WEG, k: Domain) = {
    val we = weg.asInstanceOf[WE]
    val alpha = ind2sub(dims, k.zeroBased)
    val alpha1 = new Array[Int](alpha.size)
    for (i <- 0 until alpha.size) {
      alpha1(we.he.image(Domain.zeroBased(i)).zeroBased) = ba.imageOf(we.ke(Domain.zeroBased(i)), Domain.zeroBased(alpha(i))).zeroBased
    }
    Domain.zeroBased(sub2ind(dims, alpha1))
  }
  def imagesOf0(weg: WEG) = 
    Array.tabulate(dim) ( k => imageOf(weg, Domain.zeroBased(k)).zeroBased )
  def imagesOf(weg: WEG) = 
    Array.tabulate(dim) ( k => imageOf(weg, Domain.zeroBased(k)).value )
}
