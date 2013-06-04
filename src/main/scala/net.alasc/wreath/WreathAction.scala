package net.alasc
package wreath

trait WreathAction[AE <: FiniteElement[AE], HE <: PermElement[HE]] extends Action[WreathElement[AE,HE], Perm] {
  val ba: Action[AE, Perm]
}
