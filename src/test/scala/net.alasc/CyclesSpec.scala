package net.alasc

import org.scalacheck._
import org.scalatest.FunSuite
import Arbitrary.arbitrary
import scala.collection.mutable.ArrayBuffer

object CyclesGenerators {
  val genSize = Gen.choose(1, 60)

  def cycles(n: Int) = Gen.parameterized { params =>
    import Dom.ZeroBased._
    val seq = params.rng.shuffle((0 until n).toBuffer)
    Gen.const(Perm.fromImages(n)(seq(_)).cycles)
  }

  val genCycles = for {
    n <- genSize
    c <- cycles(n)
  } yield c

  val genCyclesCycles = for {
    n <- genSize
    c1 <- cycles(n)
    c2 <- cycles(n)
  } yield (c1, c2)

  val genCyclesDomain = for {
    n <- genSize
    c <- cycles(n)
    i <- Gen.choose(1, n)
  } yield (c, Dom._1(i))

  val genCyclesCyclesDomain = for {
    n <- genSize
    c1 <- cycles(n)
    c2 <- cycles(n)
    i <- Gen.choose(1, n)
  } yield (c1, c2, Dom._1(i))
}

object CyclesSpec extends Properties("Cycles") {
  import CyclesGenerators._

  property("equals") = Prop.forAll(genCycles) {
    c => c == c.toMinimalPerm.cycles
  }
  property("hashCode") = Prop.forAll(genCycles) {
    c => c.hashCode == c.toMinimalPerm.hashCode
  }
  property("hash") = Prop.forAll(genCycles) {
    c => c.hash == c.toMinimalPerm.hash
  }

  property("Perm(n) * cycles") = Prop.forAll(genCycles) {
    c => c === Perm(c.minimalSize)(c).cycles
  }

  property("(g * g^-1) is identity") = Prop.forAll(genCycles) {
    g => (g*g.inverse).isIdentity
  }

  property("(g^-1)^-1 = g") = Prop.forAll(genCycles) {
    g => g.inverse.inverse === g
  }

  property("g === g") = Prop.forAll(genCycles) { pp => pp === pp }

  property("(g * h)^1 = h^-1 * g^-1") = Prop.forAll(genCyclesCycles) {
    case (g, h) => ((g*h).inverse) === (h.inverse)*(g.inverse)
  }

  property("{k^g}^(g^-1) = k") = Prop.forAll(genCyclesDomain) {
    case (g, k) => ((k ** g) ** (g.inverse)) === k
  }

  property("k^{g h} = {k^g}^h (right action)") = Prop.forAll(genCyclesCyclesDomain) {
    case (g, h, k) => (k ** (g * h)) === ((k ** g) ** h)
  }
}
