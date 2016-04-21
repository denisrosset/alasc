package net.alasc.bsgs

import scala.annotation.tailrec

import spire.syntax.cfor._

/** Iterable through the strong generators of a BSGS chain. */
final class StrongGeneratingSetIterable[P](val chain: Chain[P]) extends Iterable[P] {

  override def foreach[U](f: P => U): Unit = {
    @tailrec def rec(current: Chain[P]): Unit = current match {
      case node: Node[P] =>
        cforRange(0 until node.nOwnGenerators)( i => f(node.ownGenerator(i)) )
        rec(node.next)
      case _: Term[P] =>
    }
    rec(chain)
  }

  def iterator = new Iterator[P] {

    private[this] var current: Chain[P] = chain
    private[this] var index: Int = 0

    @inline @tailrec def hasNext: Boolean = current match {
      case term: Term[P] => false
      case node: Node[P] =>
        if (index == node.nOwnGenerators) {
          index = 0
          current = node.next
          hasNext
        } else true
    }

    def next: P = if (hasNext) {
      val node = current.asInstanceOf[Node[P]]
      val p = node.ownGenerator(index)
      index += 1
      p
    } else Iterator.empty.next

  }

  override def isEmpty: Boolean = size == 0

  override def size: Int = ChainRec.nStrongGenerators(chain)

}
