package net.alasc.bsgs

import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction

object IsStart {

  def unapply[G, F <: FaithfulPermutationAction[G] with Singleton](elem: Elem[G, F]): Opt[Start[G, F]] = elem match {
    case start: Start[G, F] => Opt(start)
    case _ => Opt.empty[Start[G, F]]
  }
}

object IsChain {

  def unapply[G, F <: FaithfulPermutationAction[G] with Singleton](elem: Elem[G, F]): Opt[Chain[G, F]] = elem match {
    case chain: Chain[G, F] => Opt(chain)
    case _ => Opt.empty[Chain[G, F]]
  }

}

object IsMutableStartOrNode {

  def unapply[G, F <: FaithfulPermutationAction[G] with Singleton](elem: Elem[G, F]): Opt[MutableStartOrNode[G, F]] = elem match {
    case start: Start[G, F] => Opt(start)
    case mn: MutableNode[G, F] if mn.isMutable => Opt(mn)
    case _ => Opt.empty[MutableStartOrNode[G, F]]
  }

}

object IsMutableNode {

  def unapply[G, F <: FaithfulPermutationAction[G] with Singleton](elem: Elem[G, F]): Opt[MutableNode[G, F]] = elem match {
    case mn: MutableNode[G, F] if mn.isMutable => Opt(mn)
    case _ => Opt.empty[MutableNode[G, F]]
  }

}
