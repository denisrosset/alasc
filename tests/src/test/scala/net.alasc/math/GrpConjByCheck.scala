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

  implicit def arbitraryGrp = Arbitrary {
    for {
      grp <- Grps.fromElements(Permutations.forSize[Perm](8))
      conj <- Gen.oneOf(Permutations.forSize[Perm](8).map(Some(_)), Gen.const(None))
    } yield conj.fold(grp)(by => grp.conjBy(by, by.inverse))
  }
  implicit def arbitraryPerm = Arbitrary { Permutations.forSize[Perm](9) }

  property("grp.generators.forall(g => grp.conjBy(h).contains(g.conjBy(h)))") {
    forAll { (grp: Grp[Perm], h: Perm) =>
      val hInv = h.inverse
      val conjGrp = grp.conjBy(h, hInv)
      grp.generators.forall(g => conjGrp.contains(hInv |+| g |+| h)) shouldBe true
    }
  }

  property("grp.conjBy(h).conjBy(h.inverse) == grp") {
    forAll { (grp: Grp[Perm], h: Perm) =>
      val hInv = h.inverse
      grp.conjBy(h, hInv).conjBy(hInv, h) shouldBe grp
    }
  }

  property("grp.conjBy(h1).conjBy(h2) == grp.conjBy(h1 |+| h2)") {
    forAll { (grp: Grp[Perm], h1: Perm, h2: Perm) =>
      val hInv1 = h1.inverse
      val hInv2 = h2.inverse
      grp.conjBy(h1, hInv1).conjBy(h2, hInv2) shouldBe grp.conjBy(h1 |+| h2, hInv2 |+| hInv1)
    }
  }
}
