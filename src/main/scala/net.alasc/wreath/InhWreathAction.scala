package net.alasc
package wreath

import com.faacets.coremath._

trait InhWreathAction[IWE <: InhWreathElementTrait[IWE, AE, HE], AE <: FiniteElement[AE], HE <: PermElement[HE]] extends Action[IWE, Perm] {
  val ba: Array[Action[AE, Perm]]
}
