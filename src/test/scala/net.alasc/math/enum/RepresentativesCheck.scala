package net.alasc.math
package enum

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.eq._
import spire.syntax.groupAction._
import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.subgroup._

object RepresentativesCheck extends Properties("RepresentativesCheck") with PermutationGenerators[Perm] {
  implicit def permutation = Perm.Algebra
  def genGrp(sz: Int) = for {
    gen1 <- genP(sz)
    gen2 <- genP(sz)
    gen3 <- genP(sz)
  } yield Grp(gen1, gen2, gen3)
  def genSeqGrp = for {
    sz <- Gen.choose(1, 7)
    seq <- Gen.containerOfN[Seq, Int](sz, Gen.choose(0, 3))
    grp <- genGrp(sz)
  } yield (seq, grp)
  property("All representatives are generated") = Prop.forAllNoShrink(genSeqGrp) { case (seq, grp) =>
      val bruteForceSet = grp.elements.iterator.map(g => seq <|+| g).toSet
      val cleverSeq = Representatives(seq, grp).iterator.map(_.get).toSeq
        (cleverSeq.size == bruteForceSet.size) && (cleverSeq.toSet == bruteForceSet)
  }
}
