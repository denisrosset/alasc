package net.alasc
package enum

import org.scalacheck._
import spire.algebra.Order
import spire.syntax.order._
import spire.syntax.partialAction._
import spire.std.int._

import net.alasc.finite._
import net.alasc.perms._
import net.alasc.prep._
import net.alasc.prep.chain._
import net.alasc.std.seq._
import net.alasc.laws._

abstract class RepresentativesCheck(algoName: String)(implicit builder: PGrpChainBuilder[Perm]) extends Properties("RepresentativesCheck/" + algoName) {

  def genSeq(size: Int): Gen[Seq[Int]] = Gen.containerOfN[Seq, Int](size, Gen.choose(0, 3))

  def genGrp(size: Int): Gen[Grp[Perm]] = Grps.fromElements(Permutations.forSize[Perm](size))

  def genSeqGrp: Gen[(Seq[Int], Grp[Perm])] = for {
    size <- Gen.choose(1, 6)
    seq <- genSeq(size)
    grp <- genGrp(size)
  } yield (seq, grp)

  property("All representatives are generated") =
    Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      val bruteForceSet = grp.iterator.map(g => (seq <|+|? g).get).toSet
      val cleverSeq = Representatives(seq, grp).iterator.map(_.get).toSeq
        (cleverSeq.size == bruteForceSet.size) && (cleverSeq.toSet == bruteForceSet)
    }

  property("Minimal representative is found") =
    Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      val bruteForceMinimal = grp.iterator.map(g => (seq <|+|? g).get).min(Order.ordering(spire.std.seq.SeqOrder[Int, Seq]))
      val cleverMinimal = Representatives.ordered(seq, grp).head.get
      cleverMinimal.sameElements(bruteForceMinimal)
    }

  property("Representatives are correctly retrieved by index") =
    Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      val reps = Representatives.ordered(seq, grp)
      reps.iterator.map(_.get).sameElements((0 until reps.size.toInt).iterator.map(k => reps(k).get))
    }

  property("Representatives are lexicographically ordered") =
    Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      implicit val order = spire.std.seq.SeqOrder[Int, Seq]
      val reps = Representatives.ordered(seq, grp)
      val it = reps.iterator
      var prev = it.next.get
      var correct = true
      while (it.hasNext) {
        val current = it.next.get
        if (current <= prev)
          correct = false
        prev = current
      }
      correct
    }

}

object RepresentativesCheck {

  val deterministic = {
    import PGrp.deterministic._
    implicitly[PGrpChainBuilder[Perm]]
  }

  val randomized = {
    import PGrp.default._
    implicitly[PGrpChainBuilder[Perm]]
  }

}

object RepresentativesCheckDeterministic extends RepresentativesCheck("deterministic")(RepresentativesCheck.deterministic)

object RepresentativesCheckRandomized extends RepresentativesCheck("randomized")(RepresentativesCheck.randomized)

