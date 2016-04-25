package net.alasc.tests.cyclo

import spire.math.Rational

import spire.syntax.cfor._

import cyclo.Cyclo
import org.scalacheck.{Arbitrary, Gen}

import net.alasc.laws.{AnyRefLaws, Cloner, Instances}
import net.alasc.tests.AlascSuite

class CycloSuite extends AlascSuite { // TODO: move to spire-cyclo once AnyRefLaws is in a common package

  implicit val instances: Instances[Cyclo] = Instances(Seq(Cyclo.zero, Cyclo.one, Cyclo.e(4), Cyclo.e(16)))

  implicit val cloner: Cloner[Cyclo] = Cloner { c =>
    var sum = Cyclo.zero
    val root = Cyclo.e(c.order)
    cforRange(0 until c.nTerms) { i =>
      sum += Cyclo(Rational(c.coefficient(i))) * root.pow(c.exponent(i))
    }
    sum
  }

  def genNonZeroCyclo: Gen[Cyclo] = Gen.oneOf(
    for {
      n <- Gen.choose(1, 10)
      d <- Gen.choose(1, 10)
    } yield Cyclo(Rational(n, d)),
    for {
      n <- Gen.choose(-10, -1)
      d <- Gen.choose(1, 10)
    } yield Cyclo(Rational(n, d)),
    Gen.choose(1, 5).map(k => Cyclo.e(k))
  )

  def genSimpleCyclo: Gen[Cyclo] = Gen.oneOf(
    for {
      n <- Gen.choose(-10, 10)
      d <- Gen.choose(1, 10)
    } yield Cyclo(Rational(n, d)),
    Gen.choose(1, 5).map(k => Cyclo.sinRev(Rational(1, k))),
    Gen.choose(1, 5).map(k => Cyclo.e(k))
  )


  implicit def arbCyclo: Arbitrary[Cyclo] = Arbitrary {
    Gen.oneOf(
      genSimpleCyclo,
      genNonZeroCyclo.map(_.reciprocal),
      for(x <- genSimpleCyclo; y <- genNonZeroCyclo) yield x / y,
      for(x <- genSimpleCyclo; y <- genSimpleCyclo) yield x * y,
      for(x <- genSimpleCyclo; y <- genSimpleCyclo) yield x + y,
      for(x <- genSimpleCyclo; y <- genSimpleCyclo) yield x - y
    )
  }

  checkAll("Cyclo", AnyRefLaws[Cyclo]._eq)

}
