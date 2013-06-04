package net.alasc
package bsgs

import org.scalatest.FunSuite

class RehnSuite extends FunSuite {
  test("Example 2.2 of Thomas Rehn diploma thesis") {
    import Dom.OneBased._
    val gens = List(Perm(4)(1,2), Perm(4)(2,3), Perm(4)(3,4))
    val G = Group(gens, Perm(4))
    assert(G.order == 24)
  }
  test("Example 2.6 of Thomas Rehn diploma thesis") {
    import Dom.OneBased._
    val a = Perm(5)(1,2,5)
    val b = Perm(5)(1,4)(3,5)
    val e = Perm(5)
    val t = ExpTransBuilder.empty(1, e).updated(List(a,b), List(a,b))
    assert(t.u(1) === e)
    assert(t.u(2) === a)
    assert(t.u(3) === a*a*b)
    assert(t.u(4) === b)
    assert(t.u(5) === a*a)
  }
}
