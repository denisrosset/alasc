package net.alasc.math.bsgs

import net.alasc.util._

object IsStart {
  def unapply[P](elem: Elem[P]): RefOption[Start[P]] = elem match {
    case start: Start[P] => RefSome(start)
    case _ => RefNone
  }
}

object IsChain {
  def unapply[P](elem: Elem[P]): RefOption[Chain[P]] = elem match {
    case chain: Chain[P] => RefSome(chain)
    case _ => RefNone
  }
}

object IsMutableStartOrNode {
  def unapply[P](elem: Elem[P]): RefOption[MutableStartOrNode[P]] = elem match {
    case start: Start[P] => RefSome(start)
    case mn: MutableNode[P] if mn.isMutable => RefSome(mn)
    case _ => RefNone
  }
}

object IsMutableNode {
  def unapply[P](elem: Elem[P]): RefOption[MutableNode[P]] = elem match {
    case mn: MutableNode[P] if mn.isMutable => RefSome(mn)
    case _ => RefNone
  }
}
