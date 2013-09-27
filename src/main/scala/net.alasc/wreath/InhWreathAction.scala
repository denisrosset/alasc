package net.alasc
package wreath

trait InhWreathAction[IWE <: InhWreathElementTrait[IWE, AE, HE], AE <: FiniteElement[AE], HE <: PermElement[HE]] extends Action[IWE] {
  def ba: Array[Action[AE]]
  def identity: IWE
  val faithful = ba.forall(_.faithful)
}
