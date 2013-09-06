package net.alasc
package bsgs

trait BSGSOrderings[E <: PermElement[E]] {
  self: BSGSGroup[E] =>

  lazy val domainOrder = {
    assert(isImmutable)
    val a = Array.fill[Int](degree)(-1)
    val b = base.list
    for ( (bel, i) <- b.zipWithIndex ) a(bel._0) = i
    var k = base.list.length
    for ( i <- 0 until degree ) {
      if (a(i) == -1) {
        a(i) = k
        k += 1
      }
    }
    a
  }

  object DomainOrdering extends Ordering[Dom] {
    def compare(a: Dom, b: Dom) = Ordering.Int.compare(domainOrder(a._0), domainOrder(b._0))
  }

  object BSGSOrdering extends Ordering[BSGSElement[E]] {
    def compare(a: BSGSElement[E], b: BSGSElement[E]): Int = {
      for (bel <- base.list) {
        val ord = DomainOrdering.compare(a.image(bel), b.image(bel))
        if (ord != 0)
          return ord
      }
      0
    }
  }

  object ElementOrdering extends Ordering[E] {
    def compare(a: E, b: E): Int = {
      for (bel <- base.list) {
        val ord = DomainOrdering.compare(a.image(bel), b.image(bel))
        if (ord != 0)
          return ord
      }
      0
    }
  }

  case class ImageOrdering(u: E) extends Ordering[Dom] {
    def compare(a: Dom, b: Dom) = DomainOrdering.compare(u.image(a), u.image(b))
  }

  /** Iterates through the elements of the represented group using the order
    * defined in Holt TODO:(add ref. of pages)
    */
  def orderedIterator(uPrev: E): Iterator[E] = this match {
    case g: BSGSGroupTerminal[E] => Iterator(uPrev)
    case g: BSGSGroupNode[E] => for {
      b <- g.transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
      uThis = g.transversal.u(b) * uPrev
      u <- g.tail.orderedIterator(uThis)
    } yield u
  }
}
