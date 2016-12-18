package net.alasc.tests.perms.orbits

import spire.algebra.Order
import spire.util.Opt

import org.scalacheck._

import net.alasc.bsgs.{BaseGuideLex, GrpPermAlgorithms}
import net.alasc.domains.{Domain, Partition}
import net.alasc.finite._
import net.alasc.laws._
import net.alasc.perms.orbits.Representatives
import net.alasc.perms._
import net.alasc.tests.AlascSuite
import net.alasc.tests.perms.PermSuite

abstract class RepresentativesSuite(implicit builder: GrpPermAlgorithms) extends AlascSuite {

  import builder.{baseChange, baseSwap, schreierSims}

  def genSizedArrayInt(size: Int): Gen[Array[Int]] = Gen.containerOfN[Array, Int](size, Gen.choose(0, 3))

  def genArrayInt: Gen[Array[Int]] = for {
    size <- Gen.choose(1, 7)
    array <- genSizedArrayInt(size)
  } yield array

  def genSetInt: Gen[Set[Int]] = Gen.containerOfN[Set, Int](10, Gen.choose(0, 10))

  def genSizedGrp(size: Int): Gen[Grp[Perm]] = Grps.fromElements(Permutations.permForDomain(Domain(size)))

  implicit val noShrinkArrayInt = noShrink[Array[Int]]
  implicit val noShrinkPerm = noShrink[Perm]

  test("Minimal representative is found") {
    forAll(genArrayInt) { array =>
      val n = array.length
      forAll(genSizedGrp(n)) { grp =>
        val bruteForceMinimal: Array[Int] = grp.iterator.map(g => (array <|+|? g).get).min(Order.ordering(spire.std.array.ArrayOrder[Int]))
        val grpChn = builder.fromGrp(grp, Perm.algebra, Opt(BaseGuideLex(n)))
        val minG: Perm = Representatives.findPermutationToMinimal(array, grpChn, builder.fixingPartition(grpChn, Perm.algebra, Partition.fromSeq(array))) //Representatives.ordered(seq, grp).head.get
      val cleverMinimal: Array[Int] = (array <|+|? minG).get
        cleverMinimal should ===(bruteForceMinimal)
      }
    }
  }

  test("Minimal representative is stable") {
    forAll(genArrayInt) { array =>
      val n = array.length
      forAll(genSizedGrp(n)) { grp =>
        val grpChn = builder.fromGrp(grp, Perm.algebra, Opt(BaseGuideLex(n)))
        val minG: Perm = Representatives.findPermutationToMinimal(array, grpChn, builder.fixingPartition(grpChn, Perm.algebra, Partition.fromSeq(array)))
        forAll(Grps.genRandomElement(grp)) { g =>
          val array1 = (array <|+|? g).get
          val minG1: Perm = Representatives.findPermutationToMinimal(array1, grpChn, builder.fixingPartition(grpChn, Perm.algebra, Partition.fromSeq(array1)))
          (array <|+|? minG).get should ===((array1 <|+|? minG1).get)
        }
      }
    }
  }

  test("Find permutation to another representative") {
    forAll(genArrayInt) { seq =>
      val n = seq.length
      forAll(genSizedGrp(n)) { grp =>
        val grpChn = builder.fromGrp(grp)
        val symgrp = builder.fixingPartition(grpChn, Perm.algebra, Partition.fromSeq(seq))
        forAll(Grps.genRandomElement(grp)) { g =>
          val repr = (seq <|+|? g).get
          Representatives.permutationTo(seq, repr, grpChn, symgrp) match {
            case Opt(g1) =>
              val newRepr = (seq <|+|? g1).get
              repr should ===(newRepr)
            case _ => assert(false)
          }
        }
      }
    }
  }

  test("Find potential permutation to another sequence") {
    forAll(genArrayInt) { seq =>
      val n = seq.length
      forAll(genSizedGrp(n)) { grp =>
        val grpChn = builder.fromGrp(grp, Opt(BaseGuideLex(n)))
        val symgrp = builder.fixingPartition(grpChn, Perm.algebra, Partition.fromSeq(seq))
        forAll(genSizedArrayInt(n)) { repr =>
          Representatives.permutationTo(seq, repr, grpChn, symgrp) match {
            case Opt(g) =>
              val newRepr = (seq <|+|? g).get
              repr should ===(newRepr)
            case _ =>
              grp.iterator.exists(g => (repr <|+|? g).get.sameElements(seq)) shouldBe false
          }
        }
      }
    }
  }

  test("Representatives are correctly retrieved by index") {
    forAll(genArrayInt) { seq =>
      val n = seq.length
      forAll(genSizedGrp(n)) { grp =>
        val grpChn = builder.fromGrp(grp, Perm.algebra, Opt(BaseGuideLex(n)))
        val reps = Representatives(seq, grpChn, builder.fixingPartition(grpChn, Partition.fromSeq(seq)))
        val iteratorEls = reps.orderedIterator.map(block => (seq <|+|? block.element).get.toVector)
        val indexEls = (0 until reps.size.toInt).iterator.map(i => (seq <|+|? reps(i).element).get.toVector)
        iteratorEls.toSeq shouldBe indexEls.toSeq
      }
    }
  }

  test("Representatives are lexicographically ordered") {
    implicit val ordering = Order.ordering(spire.std.array.ArrayOrder[Int])
    forAll(genArrayInt) { seq =>
      val n = seq.length
      forAll(genSizedGrp(n)) { grp =>
        val grpChn = builder.fromGrp(grp, Perm.algebra, Opt(BaseGuideLex(n)))
        val reps = Representatives(seq, grpChn, builder.fixingPartition(grpChn, Partition.fromSeq(seq)))
        val it = reps.orderedIterator
        var prev = (seq <|+|? it.next.element).get
        while (it.hasNext) {
          val current = (seq <|+|? it.next.element).get
          current shouldBe > (prev)
          prev = current
        }
      }
    }
  }
/*
  test("Minimal representative is found for sets") {
    forAll(genSetInt) { set =>
      forAll(genSizedGrp(10)) { grp =>
        import net.alasc.std.set._
        val bruteForceMinimal: Set[Int] = grp.iterator.map(g => set <|+| g).min
        val minG = orbits.OrderedSets.toSmallest(set, grp)
        val cleverMinimal: Set[Int] = set <|+| minG
        cleverMinimal should ===(bruteForceMinimal)
      }
    }
  }*/

}

final class RepresentativesSuiteDeterministic extends RepresentativesSuite()(PermSuite.deterministic)

final class RepresentativesSuiteRandomized extends RepresentativesSuite()(PermSuite.randomized)
