package net.alasc
package wreath

class InhImprimitiveAction[IWE <: InhWreathElementTrait[IWE, AE, HE], AE <: FiniteElement[AE], HE <: PermElement[HE]](val ba: Array[Action[AE]], val identity: IWE) extends InhWreathAction[IWE, AE, HE] {
  val dimensions = ba.map(_.dimension)
  val dimension = dimensions.sum
  val start = dimensions.scanLeft(0)(_+_)

  override def hashCode = 0xcafebabe + scala.util.hashing.MurmurHash3.seqHash(ba)
  override def equals(that: Any) = that match {
    case that1:InhImprimitiveAction[_, _, _] => ba.sameElements(that1.ba)
    case _ => false
  }

  def apply(we: IWE, k: Dom) = {
    val i = start.zipWithIndex.find(_._1 > k._0).get._2 - 1
    val o = k._0 - start(i)
    val i1 = we.he.image(Dom._0(i))._0
    val o1 = ba(i)(we.ke.arr(i), Dom._0(o))._0
    Dom._0(start(i1) + o1)
  }
}
