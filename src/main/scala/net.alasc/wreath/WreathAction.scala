package net.alasc
package wreath

import com.faacets.coremath._

trait WreathAction[AE <: FiniteElement[AE], HE <: PermElement[HE]] extends Action[WreathElement[AE,HE], Perm] {
  val ba: Action[AE, Perm]
}
