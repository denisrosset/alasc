package com.faacets
package perm
package wreath

import com.faacets.math._

trait InhWreathAction[AE <: FiniteElement[AE], HE <: PermElement[HE]] extends Action[InhWreathElement[AE, HE], Perm] {
  val ba: Array[Action[AE, Perm]]
}
