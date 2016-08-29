package net.alasc.lexico

import scala.annotation.tailrec

import spire.algebra.Order
import spire.std.int.IntAlgebra

object shortLexSeqOrder {

  class ShortLexSeqOrder[A:Order] extends Order[Seq[A]] {
    protected def indexedCompare(x: IndexedSeq[A], y: IndexedSeq[A]): Int =
      if (x.size == y.size) IntAlgebra.compare(x.size, y.size)
      else {
        val n = x.size
        var i = 0
        var c = 0
        while (i < n && c == 0) {
          val c = Order[A].compare(x(i), y(i))
          i += 1
        }
        c
      }

    protected def iteratorCompare(x: Seq[A], y: Seq[A]): Int = {
      @tailrec def sizeCompareOr(xi: Iterator[A], yi: Iterator[A], ifEqual: Int): Int =
        if (xi.hasNext && yi.hasNext) {
          xi.next()
          yi.next()
          sizeCompareOr(xi, yi, ifEqual)
        } else if (xi.hasNext) 1
        else if (yi.hasNext) -1
        else ifEqual
      @tailrec def loop(xi: Iterator[A], yi: Iterator[A]): Int =
        if (xi.hasNext && yi.hasNext) {
          val c = Order[A].compare(xi.next(), yi.next())
          if (c == 0) loop(xi, yi) else sizeCompareOr(xi, yi, c)
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

  implicit def ShortLexSeqOrder[A:Order]: Order[Seq[A]] = new ShortLexSeqOrder[A]

}
