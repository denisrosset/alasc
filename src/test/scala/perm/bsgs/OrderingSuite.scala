package com.faacets
package perm
package bsgs

import org.scalatest.FunSuite


class OrderingSuite extends FunSuite {
  test("Construct BSGS for M24 using deterministic/empty base and check order") {
    import M24._
    val bsgs = BSGS.schreierSims(List(a,b,c), Sym(24).identity, EmptyBase)
    for (g <- bsgs.generators)
      assert(bsgs.ElementOrdering.compare(Sym(24).identity, g.represents) < 0)
  }
}
