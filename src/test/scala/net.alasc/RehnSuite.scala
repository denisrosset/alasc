package net.alasc

import org.scalatest.FunSuite

class RehnSuite extends FunSuite {
  test("Example 2.2 of Thomas Rehn diploma thesis") {
    import Dom.OneBased._
    val g = Group(Perm(4)(1,2), Perm(4)(2,3), Perm(4)(3,4))
    assert(g.order == 24)
  }
  test("Example 2.6 of Thomas Rehn diploma thesis") {
    import Dom.OneBased._
    val a = Perm(5)(1,2,5)
    val b = Perm(5)(1,4)(3,5)
    val e = Perm(5)
    val t = TransversalExplicit.empty(1, TrivialPRepr(e)).updated(List(a,b), List(a,b))
    assert(t(1).u === e)
    assert(t(2).u === a)
    assert(t(3).u === a*a*b)
    assert(t(4).u === b)
    assert(t(5).u === a*a)
  }
}
