package com.faacets.perm
package wreath

import org.scalatest.FunSuite

class WreathSuite extends FunSuite {
  test("Can instantiate the primitive action of a wreath product group. Verify one group axiom.") {
    val W = new {
      type A = SymmetricGroup
      type H = SymmetricGroup
      val a = SymmetricGroup(3)
      val h = SymmetricGroup(3)
    } with WreathProductGroup
    val C = ActionGroup(new {
      val group = W
      val bottomAction = TrivialAction(W.a)
    } with PrimitiveWreathAction)
    val g1 = C.randomElement
    val g2 = C.randomElement
    val i1 = Permutation((g1*g2).images)
    val i2 = Permutation(g1.images)*Permutation(g2.images)
    assert(i1 == i2)
  }
}
