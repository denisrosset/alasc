package net.alasc.tests
package perms

import spire.laws.{Perm => _}
import spire.std.tuples._

import org.scalacheck.Arbitrary

import net.alasc.algebra._
import net.alasc.laws.{PermutationActionLaws, Permutations}
import net.alasc.perms._
import net.alasc.std.product._

class PermPermSuite extends AlascSuite {

  import Permutations.arbDom

  implicit def permTupleArbitrary: Arbitrary[(Perm, Perm)] =
    Arbitrary( for {
      g1 <- Permutations.forSize[Perm](16)
      g2 <- Permutations.forSize[Perm](5)
    } yield (g1, g2) )

  implicit def permPermAction: FaithfulPermutationAction[(Perm, Perm)] =
    implicitly[FaithfulPermRepBuilder[(Perm, Perm)]].build(Seq((Perm(0, 15), Perm(0, 4)))).permutationAction

  checkAll("(Perm, Perm)",      PermutationActionLaws[(Perm, Perm)].faithfulPermutationAction)

}
