package net.alasc.lexico

import scala.annotation.tailrec

import spire.algebra.Order
import spire.std.int._

object lexSeqOrder {

  // TODO: port to Spire and link implementation

  class LexSeqOrder[A:Order] extends Order[Seq[A]] {
    protected def indexedCompare(x: Seq[A], y: Seq[A]): Int = {
      val n = spire.math.min(x.size, y.size)
      var i = 0
      var c = 0
      while (i < n && c == 0) {
        c = Order[A].compare(x(i), y(i))
        i += 1
      }
      if (c != 0) c else IntAlgebra.compare(x.size, y.size)
    }

    protected def iteratorCompare(x: Seq[A], y: Seq[A]): Int = {
      @tailrec def loop(xi: Iterator[A], yi: Iterator[A]): Int =
        if (xi.hasNext && yi.hasNext) {
          val c = Order[A].compare(xi.next(), yi.next())
          if (c == 0) loop(xi, yi) else c
        } else if (xi.hasNext) 1
        else if (yi.hasNext) -1
        else 0
      loop(x.iterator, y.iterator)
    }

    def compare(x: Seq[A], y: Seq[A]): Int = x match {
      case ix: IndexedSeq[A] => y match {
        case iy: IndexedSeq[A] => indexedCompare(ix, iy)
        case _ => iteratorCompare(x, y)
      }
      case _ => iteratorCompare(x, y)
    }
  }

  implicit def LexSeqOrder[A:Order]: Order[Seq[A]] = new LexSeqOrder[A]

}
