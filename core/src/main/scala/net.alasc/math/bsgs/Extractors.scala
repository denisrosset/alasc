package net.alasc.math.bsgs

import spire.util.Nullbox

object IsStart {
  def unapply[P](elem: Elem[P]): Nullbox[Start[P]] = elem match {
    case start: Start[P] => Nullbox(start)
    case _ => Nullbox.empty[Start[P]]
  }
}

object IsChain {
  def unapply[P](elem: Elem[P]): Nullbox[Chain[P]] = elem match {
    case chain: Chain[P] => Nullbox(chain)
    case _ => Nullbox.empty[Chain[P]]
  }
}

object IsMutableStartOrNode {
  def unapply[P](elem: Elem[P]): Nullbox[MutableStartOrNode[P]] = elem match {
    case start: Start[P] => Nullbox(start)
    case mn: MutableNode[P] if mn.isMutable => Nullbox(mn)
    case _ => Nullbox.empty[MutableStartOrNode[P]]
  }
}

object IsMutableNode {
  def unapply[P](elem: Elem[P]): Nullbox[MutableNode[P]] = elem match {
    case mn: MutableNode[P] if mn.isMutable => Nullbox(mn)
    case _ => Nullbox.empty[MutableNode[P]]
  }
}
