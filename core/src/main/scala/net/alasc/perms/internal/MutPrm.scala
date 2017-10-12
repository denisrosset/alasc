package net.alasc.perms.internal

import spire.algebra.Group
import spire.syntax.cfor.cforRange

final class MutPrmGroup[I <: XInt](val i: I) extends Group[MutPrm[I]] {
  // TODO: add more
  def inverse(lhs: MutPrm[I]): MutPrm[I] = new MutPrmOps(lhs).inverse
  def empty = MutPrm(i)
  def combine(lhs: MutPrm[I], rhs: MutPrm[I]): MutPrm[I] = new MutPrmOps(lhs) |+| rhs
}

object MutPrm {

  def apply[I <: XInt](i: I): MutPrm[I] = {
    val array = new Array[Int](i)
    cforRange(0 until i) { i => array(i) = i }
    array.asInstanceOf[MutPrm[I]]
  }

  def group[I <: XInt](i: I): Group[MutPrm[I]] = new MutPrmGroup[I](i)

}

