package net.alasc.tests
package perms

import net.alasc.domains.Domain
import net.alasc.laws.{AnyRefLaws, Doms, PermutationActionLaws, Permutations}
import net.alasc.perms.{Cycle, Cycles, Perm, PermGrpChainAlgos}

class PermSuite extends AlascSuite {

  import Permutations._

  val domain = Domain(100)
  import Doms.arbDomInDomain

  checkAll("Perm", AnyRefLaws[Perm]._eq)
  checkAll("Perm", PermutationActionLaws[Perm](domain).faithfulPermutationAction)

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

/* TODO
  def permutationBuilder(implicit A: PermutationBuilder[A]) = new PermutationActionProperties(
    name = "permutation",
    parent = Some(faithfulPermutationAction),
    bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> ActionLaws[A, D].groupAction),

    "images/fromImages" -> forAll((x: A) =>
      A.fromImages(x.images(x.largestMovedPoint.getOrElseFast(-1) + 1)) === x
    )
  )

 */

object PermSuite {

  val deterministic = {
    import net.alasc.perms.deterministic._
    implicitly[PermGrpChainAlgos]
  }

  val randomized = {
    import net.alasc.perms.default._
    implicitly[PermGrpChainAlgos]
  }

}