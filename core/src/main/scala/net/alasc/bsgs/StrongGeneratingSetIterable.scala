package net.alasc.bsgs

import scala.annotation.tailrec

import spire.syntax.cfor._

/** Iterable through the strong generators of a BSGS chain. */
final class StrongGeneratingSetIterable[G](val chain: Chain[G, _]) extends Iterable[G] {

  override def foreach[U](f: G => U): Unit = {
    @tailrec def rec(current: Chain[G, _]): Unit = current match {
      case node: Node[G, _] =>
        cforRange(0 until node.nOwnGenerators)( i => f(node.ownGenerator(i)) )
        rec(node.next)
      case _: Term[G, _] =>
    }
    rec(chain)
  }

  def iterator = new Iterator[G] {

    private[this] var current: Chain[G, _] = chain
    private[this] var index: Int = 0

    @inline @tailrec def hasNext: Boolean = current match {
      case term: Term[G, _] => false
      case node: Node[G, _] =>
        if (index == node.nOwnGenerators) {
          index = 0
          current = node.next
          hasNext
        } else true
    }

    def next: G = if (hasNext) {
      val node = current.asInstanceOf[Node[G, _]]
      val p = node.ownGenerator(index)
      index += 1
      p
    } else Iterator.empty.next

  }

  override def isEmpty: Boolean = size == 0

  override def size: Int = ChainRec.nStrongGenerators(chain)

}
