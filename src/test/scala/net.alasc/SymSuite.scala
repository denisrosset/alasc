package net.alasc

import org.scalatest.FunSuite

class SymSuite extends FunSuite {
  test("Symmetric group generators are the shifts.") {
    import Dom.OneBased._
    val g1 = Perm(4)(1,2)
    val g2 = Perm(4)(2,3)
    val g3 = Perm(4)(3,4)

    val s4 = Sym(4)

    assert( s4.generators.toList.sameElements(List(g1,g2,g3)) )
  }
}
