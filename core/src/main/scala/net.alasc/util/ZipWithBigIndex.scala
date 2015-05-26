package net.alasc.util

final class RichIterator[A](val it: Iterator[A]) extends AnyVal {
  def zipWithBigIndex: Iterator[(A, BigInt)] = new Iterator[(A, BigInt)] {
    var idx: BigInt = BigInt(0)
    def hasNext = it.hasNext
    def next = {
      val ret = (it.next(), idx)
      idx += 1
      ret
    }
  }
}

trait RichIteratorOps {
  implicit def enrichIterator[A](it: Iterator[A]): RichIterator[A] = new RichIterator[A](it)
}
