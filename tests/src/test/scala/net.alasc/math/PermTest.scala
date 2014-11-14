package net.alasc.math

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.signed._
import net.alasc.syntax.permutationAction._

class PermSuite extends FunSuite with NonImplicitAssertions with Matchers with EqMatchers {
  test("For g = (1, 2, 3), 1 <* g = 2, 2 <* g = 3, 3 <* g = 1") {
    val g = Perm(1, 2, 3)
    (1 <|+| g) shouldBe 2
    (2 <|+| g) shouldBe 3
    (3 <|+| g) shouldBe 1
  }
  test("Perm conversion and Cycle.orbit") {
    val g = Perm(1, 2, 3)
    (Cycle.orbit(2, _ <|+| g).get.toCycles === g.to[Cycles]) shouldBe true
  }
  test("g1 = (1,2,3), g2 = (1,2), g1 g2 = (2,3), g2 g1 = (1,3) -- Holt 2.1.5") {
    val g1 = Perm(1, 2, 3)
    val g2 = Perm(1, 2)

    (g1 |+| g2) shouldEqv Perm(2, 3)
    (g2 |+| g1) shouldEqv Perm(1, 3)
  }
  test ("Inverse of (1, 5, 3, 6)(2, 8, 7) is (6, 3, 5, 1) (7, 8, 2) = (1, 6, 3, 5)(2, 7, 8) -- Holt 2.1.5") {
    Perm(1,5,3,6)(2,8,7).inverse shouldEqv Perm(1,6,3,5)(2,7,8)
  }
  test ("Sign of (1,3,5)(2,4) is -1 -- Wikipedia/parity of a permutation") {
    Perm(1,3,5)(2,4).signum shouldBe -1
  }
}
