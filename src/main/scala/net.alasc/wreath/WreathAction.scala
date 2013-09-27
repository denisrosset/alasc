package net.alasc
package wreath

trait WreathAction[AE <: FiniteElement[AE], HE <: PermElement[HE]] extends Action[WreathElement[AE,HE]] {
  def ba: Action[AE]
  def identity: WreathElement[AE, HE]
  def faithful = ba.faithful
}
