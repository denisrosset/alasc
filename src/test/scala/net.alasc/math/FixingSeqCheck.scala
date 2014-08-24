package net.alasc.math

import org.scalacheck._
import scala.util.Random
import org.scalatest.FunSuite

import spire.syntax.groupAction._

import net.alasc.algebra._
import bsgs._
import algorithms._
import net.alasc.std.seq._
import net.alasc.syntax.check._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.shiftablePermutation._

object FixingSeqCheck extends Properties("FixingCheck") with PermutationGenerators[Perm] {
  implicit def algebra = Perm.Algebra
  val genSeq = for {
    n <- Gen.choose(1, 30)
    seq <- Gen.containerOfN[Seq, Int](n, Gen.choose(0, 4))
  } yield seq

  val genSeqAndPerm = for {
    n <- Gen.choose(1, 30)
    seq <- Gen.containerOfN[Seq, Int](n, Gen.choose(0, 2))
    g <- genP(n)
  } yield (seq, g)
    
  property("FixingSeq") = Prop.forAllNoShrink(genSeq) { seq =>
    val subgroup = FixingSeq[Perm](seq)
    subgroup.generators.forall( g => (seq <|+| g).sameElements(seq) )
  }

  property("FixingSeq.contains") = Prop.forAllNoShrink(genSeqAndPerm) { case (seq, g) =>
      val grp1 = Grp(FixingSeq[Perm](seq).generators.toSeq:_*)
      (seq <|+| g).sameElements(seq) == grp1.contains(g)
  }
}
