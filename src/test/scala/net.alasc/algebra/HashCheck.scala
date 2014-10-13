package net.alasc.algebra

import scala.util.Random

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.algebra.Eq
import spire.syntax.eq._

trait HashCheck[H] extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  def clone(h: H): H
  implicit def eq: Eq[H]
  implicit def arb: Arbitrary[H]

  property("Clone has same hashCode") {
    forAll { (h: H) =>
      clone(h).hashCode shouldBe h.hashCode
    }
  }

  property("Equal elements have equal hashCode") {
    forAll { (g: H, h: H) =>
      ((g =!= h) || g.hashCode == h.hashCode) shouldBe true
    }
  }

  case class Eight(a: H, b: H, c: H, d: H, e: H, f: H, g: H, h: H) {
    def codes = Seq(a,b,c,d,e,f,g,h).map(_.hashCode).toSet
  }

  implicit def arb8 = Arbitrary {
    val gen = arb.arbitrary
    for(a <- gen; b <- gen; c <- gen; d <- gen; e <- gen; f <- gen; g <- gen; h <- gen) yield Eight(a,b,c,d,e,f,g,h)
  }

  property("8 random elements should not have all the same hashCode") {
    forAll { (eight: Eight) =>
      eight.codes.size should be > 1
    }
  }
}
