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
    def image(k: Dom) = {
      val i = start.zipWithIndex.find(_._1 > k._0).get._2 - 1
      val o = k._0 - start(i)
      val i1 = we.he.image(Dom._0(i))._0
      val o1 = ba(we.ke(Dom._0(i))).image(Dom._0(o))._0
      Dom._0(start(i1) + o1)
    }
    new Perm(Array.tabulate(dim) ( k => image(Dom._0(k))._0 ))
  }
}

