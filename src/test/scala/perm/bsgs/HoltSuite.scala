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
    assert( els.map(_.images1.mkString("")).sameElements(printed) )
  }
  test("Example in 4.6.2") {
    import Dom.OneBased._
    val g1 = Perm(6)(1,2,3,4)
    val g2 = Perm(6)(2,4)
    val g3 = Perm(6)(5,6)
    val g = BSGS.schreierSims(List(g1,g2,g3), Sym(6).identity, PrescribedBase(List(1,2,3,4,5,6)))
    assert(g.order == 16)
    def check(el: Perm, level: Int) =
      (el.image(1) === 1 || el.image(1) === 3) && (level < 1 || el.image(2) === 2)
    val printed = List("123456", "123465", "321456", "321465")
    val els = g.generalSearch(Sym(6).identity, 0, check).toList
    assert( els.map(_.images1.mkString("")).sameElements(printed) )
  }
}
