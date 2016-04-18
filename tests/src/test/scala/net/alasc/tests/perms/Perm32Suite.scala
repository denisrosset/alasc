package net.alasc.tests
package perms

import org.scalacheck._

import net.alasc.perms.internal.PermArray
import net.alasc.perms.{Cycles, Perm, Perm32}

class Perm32Suite extends AlascSuite {

  val genPerm32: Gen[Perm32] = for {
    n <- Gen.choose(17, 32)
    seq <- Gen.containerOfN[Seq, Int](n - 1, Gen.choose(1, 10000))
    k <- Gen.choose(0, n - 2)
    cycles = Cycles.permutationBuilder.sorting(seq) |+| Cycles(k, n - 1)
  } yield Perm32.fromHighSupportAndImageFun(cycles.movedPoints, k => k <|+| cycles, cycles.largestMovedPoint.get)

  test("Perm32 and PermArray have the same images") {
    forAll(genPerm32) { p =>
      val pa = PermArray.fromHighSupportAndImageFun(p.movedPoints, k => k <|+| (p: Perm), p.largestMovedPoint.get)
      val n = p.largestMovedPoint.get + 1
      val images1 = Seq.tabulate(n)(k => k <|+| (p: Perm))
      val images2 = Seq.tabulate(n)(k => k <|+| (pa: Perm))
      images1 should === (images2)
    }
  }

  test("Perm32 hash and PermArray hash are the same") {
    forAll(genPerm32) { p =>
      val hash1 = PermArray.fromHighSupportAndImageFun(p.movedPoints, k => k <|+| (p: Perm), p.largestMovedPoint.get).hashCode
      val hash2 = p.hashCode
      hash1 should === (hash2)
    }
  }

  test("Perm32 inverse and PermArray inverse are the same") {
    forAll(genPerm32) { p =>
      val pa = PermArray.fromHighSupportAndImageFun(p.movedPoints, k => k <|+| (p: Perm), p.largestMovedPoint.get)
      (pa.inverse: Perm) should === (p.inverse: Perm)
    }
  }

}
