package net.alasc.prep.bsgs

import scala.annotation.tailrec

// TODO: optimize base guide with a single element, and then rewrite the stabilizer(b: Int) methods
case class BaseGuideSeq(seq: Seq[Int]) extends BaseGuide {

  final class Iter(val it: Iterator[Int]) extends BaseGuideIterator {

    def hasNext = it.hasNext

    def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int =
      if (it.hasNext)
        it.next
      else
        beta

    override def checksNext(beta: Int, isFixed: Int => Boolean): Boolean =
      if (it.isEmpty)
        true
      else (it.next == beta)

  }

  def fullBase = seq

  def iterator = new Iter(seq.iterator)

}

case class BaseGuideSeqStripped(seq: Seq[Int]) extends BaseGuide {

  final class Iter(val it: Iterator[Int]) extends BaseGuideIterator {

    def hasNext = it.hasNext

    @tailrec def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int =
      if (it.hasNext) {
        val k = it.next
        if (isFixed(k))
          next(beta, easyPoints, isFixed)
        else k
      } else beta

    override def checksNext(beta: Int, isFixed: Int => Boolean): Boolean =
      if (it.isEmpty)
        true
      else {
        @tailrec def check: Boolean = {
          val k = it.next
          if (k == beta) true else {
            if (isFixed(k))
              check // continue to loop
            else false
          }
        }
        check
      }

  }

  def fullBase = seq

  def iterator = new Iter(seq.iterator)
  
}
