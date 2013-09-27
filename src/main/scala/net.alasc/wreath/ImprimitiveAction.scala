package net.alasc
package wreath

class ImprimitiveAction[AE <: FiniteElement[AE], HE <: PermElement[HE]](val ba: Action[AE], val identity: WreathElement[AE, HE]) extends WreathAction[AE, HE] {
  def dimension = ba.dimension * identity.ke.arr.size
  val dimensions = Seq.fill(identity.ke.arr.size)(ba.dimension)
  val start = dimensions.scanLeft(0)(_+_)
  def apply(we: WreathElement[AE, HE], k: Dom) = {
    val i = start.zipWithIndex.find(_._1 > k._0).get._2 - 1
    val o = k._0 - start(i)
    val i1 = we.he.image(Dom._0(i))._0
    val o1 = ba(we.ke.arr(i), Dom._0(o))._0
    Dom._0(start(i1) + o1)
  }
}
