package net.alasc.laws

import spire.algebra.{Group, Ring}

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

import net.alasc.algebra._
import net.alasc.domains.Domain
import net.alasc.perms._
import net.alasc.rep.FaithfulPermRep
import net.alasc.syntax.permutationAction._
import net.alasc.wreath._

case class WrSize(a: Int, h: Int) {

  def aPerm = Perm(0, a - 1)

  def faithfulAction: PermutationAction[Wr[Perm]] = new WrFaithfulPermutationAction[Perm](h, a)

}

object WrSize {

  implicit val arbWrSize: Arbitrary[WrSize] =
    Arbitrary(for {
      a <- Gen.choose(1, 3)
      h <- Gen.choose(1, 3)
    } yield WrSize(a, h))

}

object Wrs {

  implicit def arbWr(implicit wrSize: WrSize): Arbitrary[Wr[Perm]] =
    Arbitrary(forSize(wrSize.a, wrSize.h))

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
