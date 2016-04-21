package net.alasc.bsgs

case class BaseGuideSingle(b: Int) extends BaseGuide {

  final class Iter extends BaseGuideIterator {

    private[this] var first = true

    def hasNext = first

    def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int =
      if (first) {
        first = false
        b
      } else beta

    override def checksNext(beta: Int, isFixed: Int => Boolean): Boolean =
      if (first) b == beta else true

  }

  def fullBase = Seq(b)

  def iterator = new Iter

}
