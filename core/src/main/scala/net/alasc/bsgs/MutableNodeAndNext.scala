package net.alasc.bsgs

class MutableNodeAndNext[P](val _1: MutableNode[P]) {
  def _2: MutableNode[P] = _1.next match {
    case IsMutableNode(n) => n
    case _ => sys.error("Chain has been mutated after creation of the pair")
  }
  def isEmpty: Boolean = false
  def get: MutableNodeAndNext[P] = this
}

object MutableNodeAndNext {
  def apply[P](node1: MutableNode[P], node2: MutableNode[P]) = {
    assert(node1.next eq node2)
    assert(node2.prev eq node1)
    new MutableNodeAndNext[P](node1)
  }
  def unapply[P](mnan: MutableNodeAndNext[P]): MutableNodeAndNext[P] = mnan
}
