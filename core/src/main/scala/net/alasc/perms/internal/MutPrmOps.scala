package net.alasc.perms.internal

import spire.syntax.cfor.cforRange

class MutPrmOps[I <: XInt](val lhs: MutPrm[I]) extends AnyVal {

  def isId: Boolean = {
    cforRange(0 until lhs.length) { i =>
      if (lhs(i) != i) return false
    }
    true
  }

  def inverse: MutPrm[I] = {
    val n = lhs.length
    val array = new Array[Int](n)
    cforRange(0 until n) { k => array(lhs(k)) = k }
    array.asInstanceOf[MutPrm[I]]
  }

  def invertInPlace(rhs: MutPrm[I])(implicit s: TmpPrm[I]): Unit = {
    val n = lhs.length
    System.arraycopy(lhs, 0, s, 0, n)
    cforRange(0 until n) { i => lhs(s(i)) = i }
  }

  def |+|(rhs: MutPrm[I]): MutPrm[I] = {
    val n = lhs.length
    val array = new Array[Int](n)
    cforRange(0 until n) { i =>
      array(i) = rhs(lhs(i))
    }
    array.asInstanceOf[MutPrm[I]]
  }

}
