package net.alasc
package wreath

trait InhWreathAction[IWE <: InhWreathElementTrait[IWE, AE, HE], AE <: FiniteElement[AE], HE <: PermElement[HE]] extends Action[IWE, Perm] {
  val ba: Array[Action[AE, Perm]]
}
