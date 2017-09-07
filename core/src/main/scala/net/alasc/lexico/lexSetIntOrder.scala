package net.alasc.lexico

import scala.annotation.tailrec
import scala.collection.{SeqLike, SetLike}

import spire.algebra.Order
import spire.std.int._
import spire.math.ULong
import scala.collection.immutable.{BitSet, SortedSet}

object lexSetIntOrder {

  final class LexSetIntOrder[SI <: SetLike[Int, SI] with Set[Int]] extends Order[SI] {

    protected def nWords(bs: BitSet): Int = bs match {
      case _: BitSet.BitSet1 => 1
      case _: BitSet.BitSet2 => 2
      case ibs: collection.immutable.BitSet.BitSetN => ibs.elems.length
      case _ => bs.toBitMask.length
    }

    protected def word(bs: BitSet, w: Int): Long = bs match {
      case ibs: BitSet.BitSet1 => if (w == 0) ibs.elems else 0L
      case ibs: BitSet.BitSet2 => if (w == 0) ibs.elems0 else ibs.toBitMask(1)
      case ibs: BitSet.BitSetN => ibs.elems(w)
      case _ => bs.toBitMask(w)
    }

    /** Returns the index idx of the first nonzero word after w, idx > w, or -1 if it cannot be found. */
    @tailrec protected def firstNonZeroWordAfter(bs: BitSet, w: Int): Int = {
      require(w < nWords(bs)) // TODO remove
      if (w + 1 == nWords(bs)) -1
      else if (word(bs, w + 1) != 0L) w + 1
      else firstNonZeroWordAfter(bs, w + 1)
    }

    @tailrec protected def compareWords(x: BitSet, y: BitSet, w: Int): Int = {
      val xfnz = firstNonZeroWordAfter(x, w)
      val yfnz = firstNonZeroWordAfter(y, w)
      if (xfnz == -1) {
        if (yfnz == -1)
          0 // both bitsets after w are empty, so they are equal
        else
          -1 // y has still elements, so it's bigger than x
      } else {
        if (yfnz == -1)
          1 // x has still elements while y is empty, so x is bigger
        else {
          if (xfnz == yfnz) {
            val xwr = ULong(java.lang.Long.reverse(word(x, xfnz)))
            val ywr = ULong(java.lang.Long.reverse(word(y, yfnz)))
            val c = xwr.compare(ywr)
            // whoever has the first bit set starting from the right is going to be greater in ULong compare, but is
            // actually smaller for the lexicographic order
            if (c != 0) -c else compareWords(x, y, xfnz)
          } else if (xfnz < yfnz) -1 // x is smaller because it has the first word nonzero
          else 1 // y is smaller because it has the first word nonzero
        }
      }
    }

    @tailrec protected def compareIterators(xi: Iterator[Int], yi: Iterator[Int]): Int =
      if (xi.hasNext && yi.hasNext) {
        val c = Order[Int].compare(xi.next(), yi.next())
        if (c == 0) compareIterators(xi, yi) else c
      } else if (xi.hasNext) 1
      else if (yi.hasNext) -1
      else 0

    override def compare(x: SI, y: SI): Int = x match {
      case xbs: BitSet => y match {
        case ybs: BitSet => compareWords(xbs, ybs, -1)
        case ys: collection.SortedSet[Int] if ys.ordering eq Ordering.Int => compareIterators(xbs.iterator, ys.iterator)
        case _ => compareIterators(xbs.iterator, (SortedSet.empty[Int] ++ y).iterator)
      }
      case xs: collection.SortedSet[Int] if xs.ordering eq Ordering.Int => y match {
        case ys: collection.SortedSet[Int] if ys.ordering eq Ordering.Int => compareIterators(xs.iterator, ys.iterator)
        case _ => compareIterators(xs.iterator, (SortedSet.empty[Int] ++ y).iterator)
      }
      case _ => compareIterators((SortedSet.empty[Int] ++ x).iterator, (SortedSet.empty[Int] ++ y).iterator)
    }

  }

  implicit def LexSetIntOrder[SI <: SetLike[Int, SI] with Set[Int]]: Order[SI] = new LexSetIntOrder[SI]

}
