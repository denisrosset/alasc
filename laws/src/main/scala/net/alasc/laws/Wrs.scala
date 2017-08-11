package net.alasc.laws

import spire.algebra.{Group, Ring}

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

import net.alasc.algebra._
import net.alasc.perms._
import net.alasc.syntax.permutationAction._
import net.alasc.wreath._

object Wrs {

  def forSize(aSize: Int, hSize: Int) = for {
    aSeq <- Gen.containerOfN[Seq, Perm](hSize, Permutations.permForSize(aSize))
    h <- Permutations.permForSize(hSize)
  } yield Wr.fromPerm(aSeq.zipWithIndex.map(_.swap): _*)(h)

  def sized(implicit ev: Arbitrary[Perm]): Gen[Wr[Perm]] =
    Gen.parameterized { parameters =>
      val size = math.max(parameters.size / 10, 3)
      val aGen = Gen.resize(size, arbitrary[Perm])
      val hGen = Gen.resize(size, arbitrary[Perm])
      for {
        n <- Gen.choose(0, size)
        aSeq <- Gen.containerOfN[Seq, Perm](n, aGen)
        h <- hGen
      } yield Wr.fromPerm(aSeq.zipWithIndex.map(_.swap): _*)(h)
    }

}
