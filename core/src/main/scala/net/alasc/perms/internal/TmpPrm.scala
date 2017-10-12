package net.alasc.perms.internal

object TmpPrm {

  def apply[I <: XInt](i: I): TmpPrm[I] =
    (new Array[Int](i: Int)).asInstanceOf[TmpPrm[I]]

}
