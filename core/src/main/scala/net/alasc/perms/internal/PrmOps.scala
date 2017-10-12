package net.alasc.perms.internal

import spire.syntax.cfor.cforRange

class PrmOps(val lhs: Prm) extends AnyVal {

  def isId: Boolean = lhs.length == 0

  def inverse: Prm = {
    val n = lhs.length
    val array = new Array[Int](n)
    cforRange(0 until n) { k => array(lhs(k)) = k }
    array.asInstanceOf[Prm]
  }

  def |+|(rhs: Prm): Prm = {
    val ls = lhs.length
    val rs = rhs.length
    if (ls < rs) {
      val array = new Array[Int](rs)
      cforRange(0 until ls) { i =>
        array(i) = rhs(lhs(i))
      }
      cforRange(ls until rs) { i =>
        array(i) = rhs(i) // here lhs(i) = i
      }
      array.asInstanceOf[Prm]
    } else if (ls == rs) {
      var i = ls - 1
      while (i >= 0 && rhs(lhs(i)) == i) {
        i -= 1
      }
      val array = new Array[Int](i + 1)
      while (i >= 0) {
        array(i) = rhs(lhs(i))
        i -= 1
      }
      array.asInstanceOf[Prm]
    } else { // ls > rs
      val array = new Array[Int](ls)
      cforRange(0 until ls) { i =>
        val j = lhs(i)
        if (j >= rs)
          array(i) = j
        else
          array(i) = rhs(j)
      }
      array.asInstanceOf[Prm]
    }
  }

}
