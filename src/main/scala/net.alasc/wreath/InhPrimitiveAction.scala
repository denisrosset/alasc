package net.alasc
package wreath

class InhPrimitiveAction[IWE <: InhWreathElementTrait[IWE, AE, HE], AE <: FiniteElement[AE], HE <: PermElement[HE]](val ba: Array[Action[AE]], val identity: IWE) extends InhWreathAction[IWE, AE, HE] {
  val dimensions = ba.map(_.dimension)
  val dimension = dimensions.product

  override def hashCode = 0xbabecafe + scala.util.hashing.MurmurHash3.seqHash(ba)
  override def equals(that: Any) = that match {
    case that1:InhPrimitiveAction[_, _, _] => ba.sameElements(that1.ba)
    case _ => false
  }

  def apply(we: IWE, k: Dom) = {
    val alpha = ind2sub(dimensions, k._0)
    val alpha1 = new Array[Int](alpha.size)
    for (i <- 0 until alpha.size) {
      alpha1(we.he.image(Dom._0(i))._0) = ba(i)(we.ke.arr(i), Dom._0(alpha(i)))._0
    }
    Dom._0(sub2ind(dimensions, alpha1))
  }
}
