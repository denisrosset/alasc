package com.faacets
package perm
package wreath

import com.faacets.math.{ind2sub, sub2ind}

class InhImprimitiveAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: InhWreathGroup[A, AE, H, HE], WE <: InhWreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]](val ba: Array[Action[AE, Perm]]) extends InhWreathAction[WG, WEG, W, WE, A, AE, H, HE] {
  def dim = ba.map(_.dim).sum
  def apply(weg: WEG) = {
    val d = ba.map(_.dim)
    val we = weg.asInstanceOf[WE]
    val dim = d.sum
    def image(k: Dom) = {
      val start = d.scanLeft(0)(_+_)
      val i = start.zipWithIndex.find(_._1 > k._0).get._2 - 1
      val o = k._0 - start(i)
      val i1 = we.he.image(Dom._0(i))._0
      val o1 = ba(i)(we.ke(Dom._0(i))).image(Dom._0(o))._0
      Dom._0(start(i1) + o1)
    }
    new Perm(Array.tabulate(dim) ( k => image(Dom._0(k))._0 ))
  }
}
