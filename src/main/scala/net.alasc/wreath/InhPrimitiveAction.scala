package net.alasc
package wreath

import com.faacets.coremath.{ind2sub, sub2ind}

class InhPrimitiveAction[IWE <: InhWreathElementTrait[IWE, AE, HE], AE <: FiniteElement[AE], HE <: PermElement[HE]](val ba: Array[Action[AE, Perm]]) extends InhWreathAction[IWE, AE, HE] {
  override def hashCode = 0xbabecafe + scala.util.hashing.MurmurHash3.seqHash(ba)
  override def equals(that: Any) = that match {
    case that1:InhPrimitiveAction[_, _, _] => ba.sameElements(that1.ba)
    case _ => false
  }
//  def toTeX = TeX("\\text{Pr}") + TeX.mk(ba.map(_.toTeX), TeX("("), TeX(","), TeX(")"))
  def apply(we: IWE) = {
    val dest = (ba zip we.ke.arr).map(Function.tupled( (b, w) => b(w) ))
    val dims = dest.map(_.size)
    val dim = dims.product
    def image(k: Dom) = {
      val alpha = ind2sub(dims, k._0)
      val alpha1 = new Array[Int](alpha.size)
      for (i <- 0 until alpha.size) {
        alpha1(we.he.image(Dom._0(i))._0) = dest(i).image(Dom._0(alpha(i)))._0
      }
      Dom._0(sub2ind(dims, alpha1))
    }
    new Perm(Array.tabulate(dim)( k => image(Dom._0(k))._0))
  }
}
