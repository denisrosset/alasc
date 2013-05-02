package com.faacets
package perm
package wreath

import com.faacets.math.{ind2sub, sub2ind}

class InhImprimitiveAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: InhWreathGroup[A, AE, H, HE], WE <: InhWreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]](val g: WG, val ba: Array[Action[A, AE]]) extends InhWreathAction[WG, WEG, W, WE, A, AE, H, HE] {
  val dim = dims.sum

  def imageOf(weg: WEG, k: Domain) = {
    val we = weg.asInstanceOf[WE]
    val start = dims.scanLeft(0)(_+_)
    val i = start.zipWithIndex.find(_._1 > k.zeroBased).get._2 - 1
    val o = k.zeroBased - start(i)
    val i1 = we.he.image(Domain.zeroBased(i)).zeroBased
    val action = ba(i)
    val o1 = action.imageOf(we.ke(Domain.zeroBased(i)), Domain.zeroBased(o)).zeroBased
    Domain.zeroBased(start(i1) + o1)
  }
  def imagesOf0(weg: WEG) = 
    Array.tabulate(dim) ( k => imageOf(weg, Domain.zeroBased(k)).zeroBased )
  def imagesOf(weg: WEG) = 
    Array.tabulate(dim) ( k => imageOf(weg, Domain.zeroBased(k)).value )
}
