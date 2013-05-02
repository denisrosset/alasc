package com.faacets
package perm
package wreath

import com.faacets.math._

trait InhWreathAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: InhWreathGroup[A, AE, H, HE], WE <: InhWreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]] extends Action[WG, WEG] {
  val g: WG
  val ba: Array[Action[A, AE]]
  lazy val dims = ba.map(_.dim)
}
