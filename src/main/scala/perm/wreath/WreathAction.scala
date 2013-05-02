package com.faacets
package perm
package wreath

import com.faacets.math._

trait WreathAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: WreathGroup[A, AE, H, HE], WE <: WreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]] extends Action[WEG, Perm] {
  val ba: Action[AE, Perm]
  val d: Int
}
