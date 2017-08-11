package net.alasc.tests
package perms

import net.alasc.bsgs.{GrpChainPermutationAction}
import net.alasc.laws.{AnyRefLaws, PermutationActionLaws, Permutations}
import net.alasc.perms.{Cycle, Cycles, Perm}

class PermSuite extends AlascSuite {

  import Permutations._

  checkAll("Perm", AnyRefLaws[Perm]._eq)
  checkAll("Perm", PermutationActionLaws[Perm].faithfulPermutationAction)

  test("For g = (1, 2, 3), 1 <* g = 2, 2 <* g = 3, 3 <* g = 1") {
    val g = Perm(1, 2, 3)
    (1 <|+| g) shouldBe 2
    (2 <|+| g) shouldBe 3
    (3 <|+| g) shouldBe 1
  }

  test("Perm conversion and Cycle.orbit") {
    val g = Perm(1, 2, 3)
    (Cycle.orbit(2, _ <|+| g).get.toCycles === g.toCycles) shouldBe true
  }

  test("g1 = (1,2,3), g2 = (1,2), g1 g2 = (2,3), g2 g1 = (1,3) -- Holt 2.1.5") {
    val g1 = Perm(1, 2, 3)
    val g2 = Perm(1, 2)

    (g1 |+| g2) should === (Perm(2, 3))
    (g2 |+| g1) should === (Perm(1, 3))
  }

  test ("Inverse of (1, 5, 3, 6)(2, 8, 7) is (6, 3, 5, 1) (7, 8, 2) = (1, 6, 3, 5)(2, 7, 8) -- Holt 2.1.5") {
    (Perm(1,5,3,6)(2,8,7).inverse) should === (Perm(1,6,3,5)(2,7,8))
  }

}

object PermSuite {

  val deterministic: GrpChainPermutationAction[Perm] = {
    import net.alasc.perms.deterministic._
    permRepGrpBuilder[Perm]
  }

  val randomized: GrpChainPermutationAction[Perm] = {
    import net.alasc.perms.default._
    permRepGrpBuilder[Perm]
  }

}
