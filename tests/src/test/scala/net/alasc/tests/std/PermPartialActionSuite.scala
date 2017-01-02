package net.alasc.tests.std

import spire.laws.{ActionLaws, PartialActionLaws}

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.domains.Domain
import net.alasc.laws.Permutations
import net.alasc.perms.Perm
import net.alasc.tests.AlascSuite

class PermPartialActionSuite extends AlascSuite {

  implicit def arbPermutation: Arbitrary[Perm] = Arbitrary {
    for {
      n <- Gen.choose(1, 10)
      perm <- Permutations.permForSize(n)
    } yield perm
  }

  implicit def arbSeqInt: Arbitrary[Seq[Int]] = Arbitrary {
    for {
      n <- Gen.choose(10,20)
      array <- Gen.containerOfN[Seq, Int](n, Gen.choose(0, 10))
    } yield array
  }

  checkAll("Action[Perm, Seq[Int]]", ActionLaws[Perm, Seq[Int]].groupAction)

}
