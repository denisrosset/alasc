package net.alasc
package wreath

class InhPrimitiveAction[IWE <: InhWreathElementTrait[IWE, AE, HE], AE <: FiniteElement[AE], HE <: PermElement[HE]](val ba: Array[Action[AE]], val identity: IWE) extends InhWreathAction[IWE, AE, HE] {
  val dimensions = ba.map(_.dimension)
  val dimension = dimensions.product
  val factors = dimensions.scanLeft(1)(_*_)

  override def hashCode = 0xbabecafe + scala.util.hashing.MurmurHash3.seqHash(ba)
  override def equals(that: Any) = that match {
    case that1:InhPrimitiveAction[_, _, _] => ba.sameElements(that1.ba)
    case _ => false
  }

  def apply(we: IWE, k: Dom) = {
    val alpha1 = new Array[Int](dimensions.size)
    var rem = k._0
    var i = 0
    val n = dimensions.size
    var ind = 0
    while (i < n) {
      val alphai = rem % dimensions(i)
      rem = rem / dimensions(i)
      ind += factors(we.he.image(Dom._0(i))._0) * ba(i)(we.ke.arr(i), Dom._0(alphai))._0
      i += 1
    }
    Dom._0(ind)
  }
}
