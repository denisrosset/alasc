package net.alasc.math

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.signed._
import net.alasc.syntax.permutationAction._

class CycleSuite extends FunSuite with NonImplicitAssertions with Matchers with EqMatchers {
  test("For g = (1, 2, 3), 1 <* g = 2, 2 <* g = 3, 3 <* g = 1") {
    val g = Cycle(1, 2, 3)
    (1 <|+| g) shouldBe 2
    (2 <|+| g) shouldBe 3
    (3 <|+| g) shouldBe 1
  }
  test("Cycle construction") {
    val g = Cycle(1, 2, 3)
    Cycle.orbit(2, _ <|+| g).get shouldEqv g
  }
  test("g1 = (1,2,3), g2 = (1,2), g1 g2 = (2,3), g2 g1 = (1,3) -- Holt 2.1.5") {
    val g1 = Cycles(1, 2, 3)
    val g2 = Cycles(1, 2)

    (g1 |+| g2) shouldEqv Cycles(2, 3)
    (g2 |+| g1) shouldEqv Cycles(1, 3)
  }
  test ("Inverse of (1, 5, 3, 6)(2, 8, 7) is (6, 3, 5, 1) (7, 8, 2) = (1, 6, 3, 5)(2, 7, 8) -- Holt 2.1.5") {
    (Cycles(1,5,3,6)(2,8,7).inverse === Cycles(1,6,3,5)(2,7,8)) shouldBe true
  }
  test ("Sign of (1,3,5)(2,4) is -1 -- Wikipedia/parity of a permutation") {
    Cycles(1,3,5)(2,4).signum shouldBe -1
  }
  /*
  test ("(1,2,3) + 1 === (2,3,4)") {
    ((Cycles(1,2,3) + 1) === Cycles(2,3,4)) shouldBe true
  }
  test ("(1,2,3) - 1 === (0,1,2)") {
    ((Cycles(1,2,3) - 1) === Cycles(0,1,2)) shouldBe true
  }*/
}
