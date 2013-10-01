package net.alasc

import org.scalatest.FunSuite

class OrderingSuite extends FunSuite {
  test("Construct BSGS for M24 using deterministic Schreier-Sims and check order") {
    val group = M24.g
    for (g <- group.bsgs.strongGeneratingSet)
      assert(group.bsgs.ElementOrdering.compare(group.identity, g) < 0)
  }
}
