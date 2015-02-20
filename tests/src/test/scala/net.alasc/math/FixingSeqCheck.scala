package net.alasc.math

import org.scalacheck._
import scala.util.Random
import org.scalatest.FunSuite

import spire.syntax.partialAction._

import net.alasc.algebra._
import bsgs._
import algorithms._

import net.alasc.std.seq._
import net.alasc.syntax.all._
import net.alasc.laws._

object FixingSeqCheck extends Properties("FixingCheck") {

  implicit def permutation = Perm.Algebra
  val genSeq = for {
    n <- Gen.choose(1, 30)
    seq <- Gen.containerOfN[Seq, Int](n, Gen.choose(0, 4))
  } yield seq

  val genSeqAndPerm = for {
    n <- Gen.choose(1, 30)
    seq <- Gen.containerOfN[Seq, Int](n, Gen.choose(0, 2))
    g <- Permutations.forSize[Perm](n)
  } yield (seq, g)
    
  property("FixingSeq") = Prop.forAllNoShrink(genSeq) { seq =>
    val subgroup = FixingSeq[Perm](seq)
    subgroup.generators.forall( g => (seq <|+|? g).get.sameElements(seq) )
  }

  property("FixingSeq.contains") = Prop.forAllNoShrink(genSeqAndPerm) { case (seq, g) =>
      val grp1 = Grp(FixingSeq[Perm](seq).generators.toSeq:_*)
      (seq <|+|? g).get.sameElements(seq) == grp1.contains(g)
  }
}