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
    def image(k: Domain) = {
      val start = d.scanLeft(0)(_+_)
      val i = start.zipWithIndex.find(_._1 > k.zeroBased).get._2 - 1
      val o = k.zeroBased - start(i)
      val i1 = we.he.image(Domain.zeroBased(i)).zeroBased
      val o1 = ba(i)(we.ke(Domain.zeroBased(i))).image(Domain.zeroBased(o)).zeroBased
      Domain.zeroBased(start(i1) + o1)
    }
    new Perm(Array.tabulate(dim) ( k => image(Domain.zeroBased(k)).zeroBased ))
  }
}
