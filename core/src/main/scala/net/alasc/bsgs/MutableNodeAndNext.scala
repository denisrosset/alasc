package net.alasc.bsgs

import net.alasc.algebra.PermutationAction

/** Simple named-based extracted value to represent two consecutive mutable nodes in a BSGS chain. */
class MutableNodeAndNext[G, A <: PermutationAction[G] with Singleton](val _1: MutableNode[G, A]) {
  def _2: MutableNode[G, A] = _1.next match {
    case IsMutableNode(n) => n
    case _ => sys.error("Chain has been mutated after creation of the pair")
  }
  def isEmpty: Boolean = false
  def get: MutableNodeAndNext[G, A] = this
}

object MutableNodeAndNext {

  def apply[G, A <: PermutationAction[G] with Singleton](node1: MutableNode[G, A], node2: MutableNode[G, A]) = {
    assert(node1.next eq node2)
    assert(node2.prev eq node1)
    new MutableNodeAndNext[G, A](node1)
  }

  def unapply[G, A <: PermutationAction[G] with Singleton](mnan: MutableNodeAndNext[G, A]): MutableNodeAndNext[G, A] = mnan

}
