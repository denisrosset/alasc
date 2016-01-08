package net.alasc.prep
package bsgs

import spire.util.Opt

object IsStart {
  def unapply[P](elem: Elem[P]): Opt[Start[P]] = elem match {
    case start: Start[P] => Opt(start)
    case _ => Opt.empty[Start[P]]
  }
}

object IsChain {
  def unapply[P](elem: Elem[P]): Opt[Chain[P]] = elem match {
    case chain: Chain[P] => Opt(chain)
    case _ => Opt.empty[Chain[P]]
  }
}

object IsMutableStartOrNode {
  def unapply[P](elem: Elem[P]): Opt[MutableStartOrNode[P]] = elem match {
    case start: Start[P] => Opt(start)
    case mn: MutableNode[P] if mn.isMutable => Opt(mn)
    case _ => Opt.empty[MutableStartOrNode[P]]
  }
}

object IsMutableNode {
  def unapply[P](elem: Elem[P]): Opt[MutableNode[P]] = elem match {
    case mn: MutableNode[P] if mn.isMutable => Opt(mn)
    case _ => Opt.empty[MutableNode[P]]
  }
}
