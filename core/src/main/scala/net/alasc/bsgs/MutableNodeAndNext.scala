package net.alasc.bsgs

import net.alasc.algebra.FaithfulPermutationAction

/** Simple named-based extracted value to represent two consecutive mutable nodes in a BSGS chain. */
class MutableNodeAndNext[G, F <: FaithfulPermutationAction[G] with Singleton](val _1: MutableNode[G, F]) {
  def _2: MutableNode[G, F] = _1.next match {
    case IsMutableNode(n) => n
    case _ => sys.error("Chain has been mutated after creation of the pair")
  }
  def isEmpty: Boolean = false
  def get: MutableNodeAndNext[G, F] = this
}

object MutableNodeAndNext {

  def apply[G, F <: FaithfulPermutationAction[G] with Singleton](node1: MutableNode[G, F], node2: MutableNode[G, F]) = {
    assert(node1.next eq node2)
    assert(node2.prev eq node1)
    new MutableNodeAndNext[G, F](node1)
  }

  def unapply[G, F <: FaithfulPermutationAction[G] with Singleton](mnan: MutableNodeAndNext[G, F]): MutableNodeAndNext[G, F] = mnan

}
