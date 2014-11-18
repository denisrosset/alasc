package net.alasc.math

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.syntax.eq._
import spire.syntax.group._

import net.alasc.algebra._
import net.alasc.syntax.finiteGroup._
import net.alasc.laws._

class GrpConjByCheck extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  import AlascArbitrary._
  implicit def arbitraryGrp = Arbitrary {
    for {
      gen1 <- PermutationGen[Perm](8)
      gen2 <- PermutationGen[Perm](8)
      gen3 <- PermutationGen[Perm](8)
      conj <- Gen.oneOf(true, false)
      by <- PermutationGen[Perm](8)
    } yield if(conj) Grp(gen1, gen2, gen3).conjBy(InversePair(by, by.inverse)) else Grp(gen1, gen2, gen3)
  }
  implicit def arbitraryPerm = Arbitrary { PermutationGen[Perm](9) }

  property("grp.generators.forall(g => grp.conjBy(h).contains(g.conjBy(h)))") {
    forAll { (grp: Grp[Perm], h: Perm) =>
      val hip = InversePair(h, h.inverse)
      val conjGrp = grp.conjBy(hip)
      grp.generators.forall(g => conjGrp.contains(g.conjBy(hip))) shouldBe true
    }
  }

  property("grp.conjBy(h).conjBy(h.inverse) == grp") {
    forAll { (grp: Grp[Perm], h: Perm) =>
      val hip = InversePair(h, h.inverse)
      grp.conjBy(hip).conjBy(h.inverse) shouldBe grp
    }
  }

  property("grp.conjBy(h1).conjBy(h2) == grp.conjBy(h1 |+| h2)") {
    forAll { (grp: Grp[Perm], h1: Perm, h2: Perm) =>
      val hip1 = InversePair(h1, h1.inverse)
      val hip2 = InversePair(h2, h2.inverse)
      grp.conjBy(hip1).conjBy(hip2) shouldBe grp.conjBy(hip1 |+| hip2)
    }
  }
}
