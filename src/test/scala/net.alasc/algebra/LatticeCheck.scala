package net.alasc.algebra

import scala.util.Random

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.syntax.partialOrder._
import spire.std.boolean._

import net.alasc.syntax.lattice._

trait LatticeCheck[L] extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  implicit def lattice: Lattice[L]
  implicit def arbitrary: Arbitrary[L]

  property("Join is commutative") {
    forAll { (a: L, b: L) => (a join b) shouldEqv (b join a) }
  }

  property("Meet is commutative") {
    forAll { (a: L, b: L) => (a meet b) shouldEqv (b meet a) }
  }

  property("Join is associative") {
    forAll { (a: L, b: L, c: L) => (a join (b join c)) shouldEqv ((a join b) join c) }
  }

  property("Meet is associative") {
    forAll { (a: L, b: L, c: L) => (a meet (b meet c)) shouldEqv ((a meet b) meet c) }
  }

  property("Absorption: (a join (a meet b)) = a") {
    forAll { (a: L, b: L) => (a join (a meet b)) shouldEqv a }
  }

  property("Absorption: (a meet (a join b)) = a") {
    forAll { (a: L, b: L) => (a meet (a join b)) shouldEqv a }
  }

  property("Join is idempotent") {
    forAll { (a: L) => (a join a) shouldEqv a }
  }

  property("Meet is idempotent") {
    forAll { (a: L) => (a meet a) shouldEqv a }
  }

  property("Partial order compatible with meet") {
    forAll { (a: L, b: L) =>
      (a <= b) shouldEqv (a === (a meet b))
    }
  }

  property("Partial order compatible with join") {
    forAll { (a: L, b: L) =>
      (a <= b) shouldEqv (b === (a join b))
    }
  }
}

trait BoundedLatticeCheck[L] extends LatticeCheck[L] {
  implicit def lattice: BoundedLattice[L]

  property("(a join 1) === 1") {
    forAll { (a: L) =>
      (a join lattice.one) shouldEqv lattice.one
    }
  }

  property("(a meet 0) === 0") {
    forAll { (a: L) =>
      (a meet lattice.zero) shouldEqv lattice.zero
    }
  }

  property("a <= 1") {
    forAll { (a: L) =>
      (a <= lattice.one) shouldEqv true
    }
  }

  property("0 <= a") {
    forAll { (a: L) =>
      (lattice.zero <= a) shouldEqv true
    }
  }
}

/*
object PartitionCheck extends Properties("SubgroupSearch") {
  val genPartition2 = for {
    size <- Gen.choose(1, 60)
    seq1 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
    seq2 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
  } yield (Domain(size).Partition.fromSeq(seq1), Domain(size).Partition.fromSeq(seq2))

  val genPartition3 = for {
    size <- Gen.choose(1, 60)
    seq1 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
    seq2 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
    seq3 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
  } yield (Domain(size).Partition.fromSeq(seq1), Domain(size).Partition.fromSeq(seq2), Domain(size).Partition.fromSeq(seq3))

  property("Partition.join") = Prop.forAllNoShrink(genPartition2) { case (gpart1, gpart2) =>
    val domain = gpart1.domain
    import domain.lattice
    val domain.Partition(part1) = gpart1
    val domain.Partition(part2) = gpart2
    val jn = part1 join part2
    (part1 <= jn) && (part2 <= jn)
  }

  property("Partition.meet") = Prop.forAllNoShrink(genPartition2) { case (gpart1, gpart2) =>
    val domain = gpart1.domain
    import domain.lattice
    val domain.Partition(part1) = gpart1
    val domain.Partition(part2) = gpart2
    val mt = part1 meet part2
    (mt <= part1) && (mt <= part2)
  }
}
*/
