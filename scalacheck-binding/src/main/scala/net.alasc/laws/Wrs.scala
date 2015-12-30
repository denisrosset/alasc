package net.alasc.laws

import scala.reflect.ClassTag
import scala.util.Random

import org.scalacheck.{Arbitrary, Gen}

import spire.algebra.Group
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.math._
import wreath._
import net.alasc.syntax.permutationAction._

case class WrSize(a: Int, h: Int) {
  def aPerm[A:Permutation] = Perm(0, a - 1).to[A]
  def primitiveRepresentation[A:Group:Permutation:PermutationRepresentations, H:Permutation:PermutationRepresentations]: Representation[Wr[A, H]] = {
    val wrpr = new WrPrimitiveRepresentations[A, H]
    val aR = wrpr.aReps.get(Seq(aPerm[A]))
    wrpr.R(h, aR)
  }
  def imprimitiveRepresentation[A:Group:Permutation:PermutationRepresentations, H:Permutation:PermutationRepresentations]: Representation[Wr[A, H]] = {
    val wrir = new WrImprimitiveRepresentations[A, H]
    val aR = wrir.aReps.get(Seq(aPerm[A]))
    wrir.R(h, aR)
  }
}

object WrSize {
  implicit val arbWrSize: Arbitrary[WrSize] =
    Arbitrary(for {
      a <- Gen.choose(1, 5)
      h <- Gen.choose(1, 5)
    } yield WrSize(a, h))
  val arbWrSizeForPrimitive: Arbitrary[WrSize] =
    Arbitrary(for {
      a <- Gen.choose(2, 4)
      h <- Gen.choose(1, 4)
    } yield WrSize(a, h))
}

object Wrs {
  implicit def arbWr[A:Permutation, H:Permutation](implicit wrSize: WrSize): Arbitrary[Wr[A, H]] =
    Arbitrary(forSize(wrSize.a, wrSize.h))

  def forSize[A:Permutation, H:Permutation](aSize: Int, hSize: Int) = for {
    aSeq <- Gen.containerOfN[Seq, A](hSize, Permutations.forSize[A](aSize))
    h <- Permutations.forSize[H](hSize)
  } yield Wr(aSeq, h)

  def sized[A:Arbitrary, H:Arbitrary]: Gen[Wr[A, H]] =
    Gen.parameterized { parameters =>
      val size = math.max(parameters.size / 10, 3)
      val aGen = Gen.resize(size, implicitly[Arbitrary[A]].arbitrary)
      val hGen = Gen.resize(size, implicitly[Arbitrary[H]].arbitrary)
      for {
        n <- Gen.choose(0, size)
        aSeq <- Gen.containerOfN[Seq, A](n, aGen)
        h <- hGen
      } yield Wr(aSeq, h)
    }
}
