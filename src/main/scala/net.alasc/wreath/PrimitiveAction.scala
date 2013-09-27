package net.alasc
package wreath

class PrimitiveAction[AE <: FiniteElement[AE], HE <: PermElement[HE]](val ba: Action[AE], val identity: WreathElement[AE, HE]) extends WreathAction[AE, HE] {
  val dimensions = Seq.fill(identity.ke.arr.size)(ba.dimension)
  val dimension = BigInt(ba.dimension).pow(identity.ke.arr.size).toInt
  def apply(we: WreathElement[AE, HE], k: Dom) = {
    val alpha = ind2sub(dimensions, k._0)
    val alpha1 = new Array[Int](alpha.size)
    for (i <- 0 until alpha.size)
      alpha1(we.he.image(Dom._0(i))._0) = ba(we.ke.arr(i), Dom._0(alpha(i)))._0
    Dom._0(sub2ind(dimensions, alpha1))
  }
}
