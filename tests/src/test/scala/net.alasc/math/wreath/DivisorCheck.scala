package net.alasc.math
package wreath

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.eq._
import net.alasc.algebra._
import net.alasc.syntax.permutationAction._

class DivisorCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  case class NonNeg(value: Int)
  implicit def arbitrary: Arbitrary[NonNeg] = Arbitrary {
    Gen.choose(0, Int.MaxValue).map(NonNeg(_))
  }
  property("Divisor works correctly") {
    forAll { (x: NonNeg, y: NonNeg, d: NonNeg) =>
      Divisor(y.value, d.value).divide(x.value) shouldBe (x.value / d.value)
    }
  }
}
