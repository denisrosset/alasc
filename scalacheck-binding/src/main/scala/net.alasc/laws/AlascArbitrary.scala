package net.alasc.laws

import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Gen}

import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.cfor._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.math._
import net.alasc.math.wreath._
import net.alasc.syntax.permutationAction._

object AlascArbitrary {
  /** Returns an array of images representing a permutation. */
  def randomImages(domainSize: Int, rng: scala.util.Random): Array[Int] = {
    // uses the Fisher-Yates shuffle, inside out variant
    val array = new Array[Int](domainSize)
    cforRange(0 until domainSize) { i =>
      val j = rng.nextInt(i + 1)
      array(i) = array(j)
      array(j) = i
    }
    array
  }

  def PermutationGen[P : Permutation]: Gen[P] = Gen.parameterized { parameters =>
    val images = randomImages(parameters.size, parameters.rng)
    Permutation[P].fromImages(images)
  }

  def PermutationGen[P : Permutation](domainSize: Int) = Gen.parameterized { parameters =>
    val images = randomImages(domainSize, parameters.rng)
    Permutation[P].fromImages(images)
  }

  implicit def DomArbitrary: Arbitrary[Dom] = Arbitrary { Gen.choose(0, 100).map(Dom(_)) }

  implicit def PermutationArbitrary[P : Permutation]: Arbitrary[P] = Arbitrary { PermutationGen[P] }

  implicit def GrpPermutationArbitrary[P : Permutation : PermutationRepresentations : ClassTag]: Arbitrary[Grp[P]] = Arbitrary {
    val domainSize = 8
    for {
      gen1 <- PermutationGen(domainSize)
      gen2 <- PermutationGen(domainSize)
      gen3 <- PermutationGen(domainSize)
    } yield Grp(gen1, gen2, gen3)
  }

  def genPartition(domain: Domain): Gen[domain.Partition] =
    for {
      seq <- Gen.containerOfN[Seq, Int](domain.size, Gen.oneOf(0,1,2,3))
    } yield domain.Partition.fromSeq(seq)

  implicit def PartitionArbitrary(domain: Domain): Arbitrary[domain.Partition] = Arbitrary(genPartition(domain))
  def genDomain = Gen.parameterized { parameters => Domain(parameters.size) }
  def genPartitionMap[V : Arbitrary : ClassTag](domain: Domain): Gen[domain.PartitionMap[V]] =
    for {
      partition <- genPartition(domain)
      values <- Gen.containerOfN[Seq, V](partition.blocks.size, implicitly[Arbitrary[V]].arbitrary)
    } yield domain.PartitionMap.tabulate(partition)(block => values(partition.blockIndex(block.min)))
  implicit def PartitionMapArbitrary[V : Arbitrary : ClassTag](domain: Domain): Arbitrary[domain.PartitionMap[V]] = Arbitrary {
    genPartitionMap(domain)
  }
  implicit def PartitionArbitrary: Arbitrary[Domain#Partition] = Arbitrary {
    genDomain.flatMap(domain => genPartition(domain))
  }
  implicit def PartitionMapArbitrary[V : Arbitrary : ClassTag]: Arbitrary[Domain#PartitionMap[V]] = Arbitrary {
    genDomain.flatMap(domain => genPartitionMap[V](domain))
  }
  val wrSize = 4
  val wrPermSize = 4
  implicit def genWrPermPerm: Gen[Wr[Perm, Perm]] = for {
    n <- Gen.choose(0, wrSize)
    aSeq <- Gen.containerOfN[Seq, Perm](n, PermutationGen[Perm](wrPermSize))
    h <- PermutationGen[Perm](wrPermSize)
  } yield Wr(aSeq, h)
  implicit def WrPermPerm: Arbitrary[Wr[Perm, Perm]] = Arbitrary(genWrPermPerm)
}
