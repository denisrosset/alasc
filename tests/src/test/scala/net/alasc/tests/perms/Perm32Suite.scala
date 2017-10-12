package net.alasc.tests
package perms

import net.alasc.laws.{AnyRefLaws, PermutationActionLaws}
import net.alasc.perms.sized.Perm32
import net.alasc.laws.Permutations._
import org.scalacheck._
import net.alasc.perms.{Cycles, Perm}

class Perm32Suite extends AlascSuite {

  checkAll("Perm32", AnyRefLaws[Perm32]._eq)
  checkAll("Perm32", PermutationActionLaws[Perm32].faithfulPermutationAction)

  /*
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
    }*/

}
