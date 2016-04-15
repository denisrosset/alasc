package net.alasc.wreath

import org.scalacheck._
import org.scalatest._
import prop._

case class NonNeg(value: Int) extends AnyVal

object NonNeg {

  implicit def arbitrary: Arbitrary[NonNeg] = Arbitrary {
    Gen.choose(0, Int.MaxValue).map(NonNeg(_))
  }

}

class DivisorCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {

  property("Divisor works correctly") {
    forAll { (x: NonNeg, y: NonNeg, d: NonNeg) =>
      Divisor(y.value, d.value).divide(x.value) shouldBe (x.value / d.value)
    }
  }

}
