package net.alasc.bsgs

import spire.util.Opt

import net.alasc.algebra.PermutationAction

object IsStart {

  def unapply[G, A <: PermutationAction[G] with Singleton](elem: Elem[G, A]): Opt[Start[G, A]] = elem match {
    case start: Start[G, A] => Opt(start)
    case _ => Opt.empty[Start[G, A]]
  }
}

object IsChain {

  def unapply[G, A <: PermutationAction[G] with Singleton](elem: Elem[G, A]): Opt[Chain[G, A]] = elem match {
    case chain: Chain[G, A] => Opt(chain)
    case _ => Opt.empty[Chain[G, A]]
  }

}

object IsMutableStartOrNode {

  def unapply[G, A <: PermutationAction[G] with Singleton](elem: Elem[G, A]): Opt[MutableStartOrNode[G, A]] = elem match {
    case start: Start[G, A] => Opt(start)
    case mn: MutableNode[G, A] if mn.isMutable => Opt(mn)
    case _ => Opt.empty[MutableStartOrNode[G, A]]
  }

}

object IsMutableNode {

  def unapply[G, A <: PermutationAction[G] with Singleton](elem: Elem[G, A]): Opt[MutableNode[G, A]] = elem match {
    case mn: MutableNode[G, A] if mn.isMutable => Opt(mn)
    case _ => Opt.empty[MutableNode[G, A]]
  }

}
