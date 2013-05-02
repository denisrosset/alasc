package com.faacets
package perm
package wreath

import com.faacets.math.{ind2sub, sub2ind}

class InhPrimitiveAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: InhWreathGroup[A, AE, H, HE], WE <: InhWreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]](val ba: Array[Action[AE, Perm]]) extends InhWreathAction[WG, WEG, W, WE, A, AE, H, HE] {
  def dim = ba.map(_.dim).product
  def apply(weg: WEG) = {
    val we = weg.asInstanceOf[WE]
    val d = ba.map(_.dim)
    val dim = d.product
    def image(k: Dom) = {
      val alpha = ind2sub(d, k._0)
      val alpha1 = new Array[Int](alpha.size)
      for (i <- 0 until alpha.size) {
        val action = ba(i)
        alpha1(we.he.image(Dom._0(i))._0) = ba(i)(we.ke(Dom._0(i))).image(Dom._0(alpha(i)))._0
      }
      Dom._0(sub2ind(d, alpha1))
    }
    new Perm(Array.tabulate(dim)( k => image(Dom._0(k))._0))
  }
}
