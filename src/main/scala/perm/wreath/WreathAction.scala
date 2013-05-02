package com.faacets
package perm
package wreath

import com.faacets.math._

trait WreathAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: WreathGroup[A, AE, H, HE], WE <: WreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]] extends Action[WG, WEG] {
  val g: WG
  val ba: Action[A, AE]
  lazy val dims = Array.fill[Int](g.asInstanceOf[W].h.degree)(ba.dim)
}
