package com.faacets
package perm
package wreath

import com.faacets.math._

trait InhWreathAction[WG <: FiniteGroup[WEG], WEG <: FiniteElement[WEG], W <: InhWreathGroup[A, AE, H, HE], WE <: InhWreathElement[AE, HE],
  A <: FiniteGroup[AE], AE <: FiniteElement[AE],
  H <: PermGroup[HE], HE <: PermElement[HE]] extends Action[WEG, Perm] {
  val ba: Array[Action[AE, Perm]]
}
