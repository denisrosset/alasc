package net.alasc.tests.wreath

import org.scalacheck._

import net.alasc.tests.AlascSuite
import net.alasc.wreath.Divisor

case class NonNeg(value: Int) extends AnyVal

object NonNeg {

  implicit def arbitrary: Arbitrary[NonNeg] = Arbitrary {
    Gen.choose(0, Int.MaxValue).map(NonNeg(_))
  }

}

class DivisorCheck extends AlascSuite {

  test("Divisor works correctly") {
    forAll { (x: NonNeg, y: NonNeg, d: NonNeg) =>
      Divisor(y.value, d.value).divide(x.value) should === (x.value / d.value)
    }
  }

}
