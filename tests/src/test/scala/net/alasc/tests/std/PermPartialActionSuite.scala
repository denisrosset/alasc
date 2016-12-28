package net.alasc.tests.std

import spire.laws.PartialActionLaws

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

  implicit def arbArrayInt: Arbitrary[Array[Int]] = Arbitrary {
    for {
      n <- Gen.choose(1,20)
      array <- Gen.containerOfN[Array, Int](n, Gen.choose(0, 10))
    } yield array
  }

  implicit def arbSeqInt: Arbitrary[Seq[Int]] = Arbitrary {
    for {
      n <- Gen.choose(1,20)
      array <- Gen.containerOfN[Seq, Int](n, Gen.choose(0, 10))
    } yield array
  }

  checkAll("PartialAction[Perm, Array[Int]]", PartialActionLaws[Perm, Array[Int]].groupPartialAction)

  checkAll("PartialAction[Perm, Seq[Int]]", PartialActionLaws[Perm, Seq[Int]].groupPartialAction)

}
