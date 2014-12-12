package net.alasc.math
package enum

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.algebra.Order
import spire.syntax.eq._
import spire.syntax.order._
import spire.syntax.action._
import spire.std.int._
import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.all._
import net.alasc.laws._

object RepresentativesCheck extends Properties("RepresentativesCheck") {
  import AlascArbitrary._
  def genGrp(sz: Int) = for {
    gen1 <- PermutationGen[Perm](sz)
    gen2 <- PermutationGen[Perm](sz)
    gen3 <- PermutationGen[Perm](sz)
  } yield Grp(gen1, gen2, gen3)
  def genSeqGrp = for {
    sz <- Gen.choose(1, 8)
    seq <- Gen.containerOfN[Seq, Int](sz, Gen.choose(0, 3))
    grp <- genGrp(sz)
  } yield (seq, grp)
  property("All representatives are generated") = Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      val bruteForceSet = grp.elements.iterator.map(g => seq <|+|! g).toSet
      val cleverSeq = Representatives(seq, grp).iterator.map(_.get).toSeq
        (cleverSeq.size == bruteForceSet.size) && (cleverSeq.toSet == bruteForceSet)
  }
  property("Minimal representative is found") = Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      val bruteForceMinimal = grp.elements.iterator.map(g => seq <|+|! g).min(Order.ordering(spire.std.seq.SeqOrder[Int, Seq]))
      val cleverMinimal = RepresentativesOrdered(seq, grp).head.get
      cleverMinimal.sameElements(bruteForceMinimal)
  }
  property("Representatives are correctly retrieved by index") = Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      val reps = RepresentativesOrdered(seq, grp)
      reps.iterator.map(_.get).sameElements((0 until reps.size.toInt).iterator.map(k => reps(k).get))
  }
  property("Representatives are lexicographically ordered") = Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      implicit val order = spire.std.seq.SeqOrder[Int, Seq]
      val reps = RepresentativesOrdered(seq, grp)
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
