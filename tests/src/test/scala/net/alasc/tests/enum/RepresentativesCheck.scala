package net.alasc.tests
package enum

import spire.algebra.Order
import spire.std.int._
import spire.syntax.partialAction._
import spire.util.Opt

import org.scalacheck._

import net.alasc.bsgs.BaseGuideLex
import net.alasc.domains.Partition
import net.alasc.enum.Representatives
import net.alasc.finite._
import net.alasc.laws._
import net.alasc.perms._
import net.alasc.perms.chain.PermGrpChainBuilder
import net.alasc.std.array._
import net.alasc.tests.bsgs.BSGSSuite

abstract class RepresentativesSuite(implicit builder: PermGrpChainBuilder[Perm]) extends AlascSuite {

  def genSizedArrayInt(size: Int): Gen[Array[Int]] = Gen.containerOfN[Array, Int](size, Gen.choose(0, 3))

  def genArrayInt: Gen[Array[Int]] = for {
    size <- Gen.choose(1, 7)
    array <- genSizedArrayInt(size)
  } yield array

  def genSizedGrp(size: Int): Gen[Grp[Perm]] = Grps.fromElements(Permutations.forSize[Perm](size))

  implicit val noShrinkArrayInt = noShrink[Array[Int]]
  implicit val noShrinkPerm = noShrink[Perm]
  /*
  test("All representatives are generated") =
    Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      val bruteForceSet = grp.iterator.map(g => (seq <|+|? g).get).toSet
      val cleverSeq = Representatives(seq, grp).iterator.map(_.get).toSeq
        (cleverSeq.size == bruteForceSet.size) && (cleverSeq.toSet == bruteForceSet)
    }
*/

  test("Minimal representative is found") {
    forAll(genArrayInt) { array =>
      val n = array.length
      forAll(genSizedGrp(n)) { grp =>
        val bruteForceMinimal: Array[Int] = grp.iterator.map(g => (array <|+|? g).get).min(Order.ordering(spire.std.array.ArrayOrder[Int]))
        val minG: Perm = Representatives.findPermutationToMinimal(array, builder.fromGrp(grp, Opt(BaseGuideLex(n))).chain, grp.fixingPartition(Partition.fromSeq(array))) //Representatives.ordered(seq, grp).head.get
      val cleverMinimal: Array[Int] = (array <|+|? minG).get
        cleverMinimal should ===(bruteForceMinimal)
      }
    }
  }

  test("Minimal representative is stable") {
    forAll(genArrayInt) { array =>
      val n = array.length
      forAll(genSizedGrp(n)) { grp =>
        val chn = builder.fromGrp(grp, Opt(BaseGuideLex(n))).chain
        val minG: Perm = Representatives.findPermutationToMinimal(array, chn, grp.fixingPartition(Partition.fromSeq(array)))
        forAll(Grps.genRandomElement(grp)) { g =>
          val array1 = (array <|+|? g).get
          val minG1: Perm = Representatives.findPermutationToMinimal(array1, chn, grp.fixingPartition(Partition.fromSeq(array1)))
          (array <|+|? minG).get should ===((array1 <|+|? minG1).get)
        }
      }
    }
  }

  test("Find permutation to another representative") {
    forAll(genArrayInt) { seq =>
      val n = seq.length
      forAll(genSizedGrp(n)) { grp =>
        val chain = builder.fromGrp(grp).chain
        val symgrp = grp.fixingPartition(Partition.fromSeq(seq))
        forAll(Grps.genRandomElement(grp)) { g =>
          val repr = (seq <|+|? g).get
          Representatives.permutationTo(seq, repr, chain, symgrp) match {
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
        val chain = builder.fromGrp(grp, Opt(BaseGuideLex(n))).chain
        val symgrp = grp.fixingPartition(Partition.fromSeq(seq))
        forAll(genSizedArrayInt(n)) { repr =>
          Representatives.permutationTo(seq, repr, chain, symgrp) match {
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
        val reps = Representatives(seq, grp)
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
        val reps = Representatives(seq, grp)
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

}

final class RepresentativesSuiteDeterministic extends RepresentativesSuite()(BSGSSuite.deterministic)

final class RepresentativesSuiteRandomized extends RepresentativesSuite()(BSGSSuite.randomized)
