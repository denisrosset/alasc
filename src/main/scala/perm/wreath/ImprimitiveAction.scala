package com.faacets
package perm
package wreath

import com.faacets.math.{ind2sub, sub2ind}

class ImprimitiveAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: WreathGroup[A, AE, H, HE], WE <: WreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]](val ba: Action[AE, Perm], val n: Int) extends WreathAction[WG, WEG, W, WE, A, AE, H, HE] {
  def dim = ba.dim*n
  def apply(weg: WEG) = {
    val we = weg.asInstanceOf[WE]
    val d = ba.dim
    val dims = Array.fill[Int](we.he.size)(d)
    val dim = dims.sum
    val start = dims.scanLeft(0)(_+_)
    def image(k: Domain) = {
      val i = start.zipWithIndex.find(_._1 > k.zeroBased).get._2 - 1
      val o = k.zeroBased - start(i)
      val i1 = we.he.image(Domain.zeroBased(i)).zeroBased
      val o1 = ba(we.ke(Domain.zeroBased(i))).image(Domain.zeroBased(o)).zeroBased
      Domain.zeroBased(start(i1) + o1)
    }
    new Perm(Array.tabulate(dim) ( k => image(Domain.zeroBased(k)).zeroBased ))
  }
}

