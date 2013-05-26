package com.faacets
package perm
package bsgs

import org.scalatest.FunSuite


class HoltSuite extends FunSuite {
  test("Example 4.4") {
    import Dom.OneBased._
    val g1 = Perm(6)(1,2,3,4)
    val g2 = Perm(6)(2,4)
    val g3 = Perm(6)(5,6)
    val g = BSGS.schreierSims(List(g1,g2,g3), Sym(6).identity)
    assert(g.order == 16)
    val els: List[Perm] = g.orderedIterator(Sym(6).identity).toList
    assert( (els zip els.tail).forall( Function.tupled( (a,b) => g.ElementOrdering.compare(a,b) < 0 ) ) )
    val printed = List("123456", "123465", "143256", "143265", "214356", "214365", "234156", "234165",
      "321456", "321465", "341256", "341265", "412356", "412365", "432156", "432165")
    assert( els.map(_.images.oneBased.mkString("")).sameElements(printed) )
  }
  test("Example in 4.6.2") {
    import Dom.OneBased._
    val g1 = Perm(6)(1,2,3,4)
    val g2 = Perm(6)(2,4)
    val g3 = Perm(6)(5,6)
    val g = BSGS.schreierSims(List(g1,g2,g3), Sym(6).identity, PrescribedBase(List(1,2,3,4,5,6)))
    assert(g.order == 16)
    def test(partialBaseImage: List[Dom], level: Int): Boolean = {
      val b = partialBaseImage.head
      if (level == 0)
        return b === 1 || b === 3
      if (level == 1)
        return b === 2
      return true
    }
    def predicate(k: Perm) = (k.image(1) === 1 || k.image(1) === 3) && k.image(2) === 2
    val printed = List("123456", "123465", "321456", "321465")
    val els = g.generalSearch(predicate, test).toList
    assert( els.map(_.images.oneBased.mkString("")).sameElements(printed) )
  }
}
