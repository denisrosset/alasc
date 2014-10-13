package net.alasc.math

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.algebra.Eq
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.eq._
import spire.std.int._
import spire.std.seq._

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._
import perm._

class Perm32Check extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  val genPerm32: Gen[Perm32] = for {
    n <- Gen.choose(17, 32)
    seq <- Gen.containerOfN[Seq, Int](n - 1, Gen.choose(1, 10000))
    k <- Gen.choose(0, n - 2)
    cycles = Cycles.Algebra.sorting(seq) |+| Cycles(k, n - 1)
  } yield Perm32.fromHighSupportAndImageFun(cycles.support, k => k <|+| cycles, cycles.supportMax.get)

  implicit def arbitraryPerm32: Arbitrary[Perm32] = Arbitrary(genPerm32)
  property("Perm32 and PermArray have the same images") {
    forAll { (p: Perm32) =>
      val pa = PermArray.fromHighSupportAndImageFun(p.support, k => k <|+| (p: Perm), p.supportMax.get)
      val n = p.supportMax.get + 1
      val images1 = Seq.tabulate(n)(k => k <|+| (p: Perm))
      val images2 = Seq.tabulate(n)(k => k <|+| (pa: Perm))
      images1 shouldEqv images2
    }
  }

  property("Perm32 hash and PermArray hash are the same") {
    forAll { (p: Perm32) =>
      val hash1 = PermArray.fromHighSupportAndImageFun(p.support, k => k <|+| (p: Perm), p.supportMax.get).hashCode
      val hash2 = p.hashCode
      hash1 shouldEqv hash2
    }
  }
  property("Perm32 inverse and PermArray inverse are the same") {
    forAll { (p: Perm32) =>
      val pa = PermArray.fromHighSupportAndImageFun(p.support, k => k <|+| (p: Perm), p.supportMax.get)
      (pa.inverse: Perm) shouldEqv (p.inverse: Perm)
    }
  }
}
