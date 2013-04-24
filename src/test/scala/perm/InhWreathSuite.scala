package com.faacets.perm
package wreath

import org.scalatest.FunSuite

class InhWreathSuite extends FunSuite {
  test("Can instantiate the primitive action of a inhomogenous wreath product group. Verify one group axiom.") {
    val W = new {
      type A = SymmetricGroup
      type H = SymmetricGroup
      val aArr = Array(SymmetricGroup(3), SymmetricGroup(3), SymmetricGroup(3)).asInstanceOf[Array[AnyRef]]
      val h = SymmetricGroup(3)
    } with InhWreathProductGroup
    val C = ActionGroup(new {
      val group = W
      val bottomActionArr = group.aArr.map(x => TrivialAction(x.asInstanceOf[SymmetricGroup])).asInstanceOf[Array[AnyRef]]
    } with InhImprimitiveWreathAction)
    val g1 = C.randomElement
    val g2 = C.randomElement
    val i1 = Permutation((g1*g2).images)
    val i2 = Permutation(g1.images)*Permutation(g2.images)
    assert(i1 == i2)
  }
}
