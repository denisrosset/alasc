package net.alasc.tests
package perms

import net.alasc.perms.{Cycle, Cycles}

class CycleSuite extends AlascSuite {

  test("For g = (1, 2, 3), 1 <* g = 2, 2 <* g = 3, 3 <* g = 1") {
    val g = Cycle(1, 2, 3)
    (1 <|+| g) shouldBe 2
    (2 <|+| g) shouldBe 3
    (3 <|+| g) shouldBe 1
  }

  test("Cycle construction") {
    val g = Cycle(1, 2, 3)
    (Cycle.orbit(2, _ <|+| g).get: Cycle) should === (g: Cycle)
  }

  test("g1 = (1,2,3), g2 = (1,2), g1 g2 = (2,3), g2 g1 = (1,3) -- Holt 2.1.5") {
    val g1 = Cycles(1, 2, 3)
    val g2 = Cycles(1, 2)

    (g1 |+| g2) should === (Cycles(2, 3))
    (g2 |+| g1) should === (Cycles(1, 3))
  }

  test ("Inverse of (1, 5, 3, 6)(2, 8, 7) is (6, 3, 5, 1) (7, 8, 2) = (1, 6, 3, 5)(2, 7, 8) -- Holt 2.1.5") {
    Cycles(1,5,3,6)(2,8,7).inverse should === (Cycles(1,6,3,5)(2,7,8))
  }

}
