package com.faacets.perm

import org.scalatest.FunSuite
import org.scalacheck._
import com.faacets.perm._

class SchreierSimsConstructionSuite extends FunSuite {
  test("Example 2.2 of Thomas Rehn diploma thesis") {
    val g01 = ExplicitPermutation(4)(0,1)
    val g12 = ExplicitPermutation(4)(1,2)
    val g23 = ExplicitPermutation(4)(2,3)
    val G = SchreierSimsConstruction.construct(List(3,2,1), List(g01,g12,g23), ExplicitTransversal.fromGenerators[ExplicitPermutation])
    assert(G.order == 24)
  }
  test("Rubik cube group order (example from GAP system)") {
    // http://www.gap-system.org/Doc/Examples/rubik.html
    val g1 = ExplicitPermutation(48)(0,2,7,5)(1,4,6,3)(8,32,24,16)(9,33,25,17)(10,34,26,18)
    val g2 = ExplicitPermutation(48)(8,10,15,13)(9,12,14,11)(0,16,40,39)(3,19,43,36)(5,21,45,34)
    val g3 = ExplicitPermutation(48)(16,18,23,21)(17,20,22,19)(5,24,42,15)(6,27,41,12)(7,29,40,10)
    val g4 = ExplicitPermutation(48)(24,26,31,29)(25,28,30,27)(2,37,42,18)(4,35,44,20)(7,32,47,23)
    val g5 = ExplicitPermutation(48)(32,34,39,37)(33,36,38,35)(2,8,45,31)(1,11,46,28)(0,13,47,26)
    val g6 = ExplicitPermutation(48)(40,42,47,45)(41,44,46,43)(13,21,29,37)(14,22,30,38)(15,23,31,39)
    val G = SchreierSimsConstruction.construct(List.empty[Domain], List(g1,g2,g3,g4,g5,g6), ExplicitTransversal.fromGenerators[ExplicitPermutation])
    // cannot use method "order" because the order exceeds platform integer size!
    val order = (G.transversals :\ (1:BigInt))((kv, p: BigInt) => kv.size*(p:BigInt))
    assert(order === BigInt("43252003274489856000"))
  }
}
