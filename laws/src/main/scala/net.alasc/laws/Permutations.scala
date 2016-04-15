package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}

import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.perms._
import net.alasc.syntax.permutationAction._

/** Generators and arbitraries for permutations.
  * 
  * Note: by default, permutations are generated using the size provided
  * by ScalaTest as maximal domain size.
  * 
  * When used e.g. as generators for a random group, these generators
  * should be resized.
  */
object Permutations {
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

  def sized[P:PermutationBuilder]: Gen[P] = Gen.parameterized { parameters =>
    val images = randomImages(parameters.size, parameters.rng)
    PermutationBuilder[P].fromImages(images)
  }

  def forSize[P:PermutationBuilder](domainSize: Int) = Gen.parameterized { parameters =>
    val images = randomImages(domainSize, parameters.rng)
    PermutationBuilder[P].fromImages(images)
  }

  def domSized: Gen[Dom] = Gen.posNum[Int].map(Dom(_))

  def domForSize(size: Int): Gen[Dom] = Gen.choose(0, size - 1).map(Dom(_))

  implicit def arbPermutation[P : PermutationBuilder]: Arbitrary[P] = Arbitrary(sized[P])
  implicit def arbDom: Arbitrary[Dom] = Arbitrary(domSized)
  implicit def permutationInstances[P](implicit P: PermutationBuilder[P]): Instances[P] =
    Instances[P](Seq(Perm(0,1).to[P], P.id))
  implicit def permutationCloner[P](implicit P: PermutationBuilder[P]): Cloner[P] =
    Cloner( (p: P) => P.fromImages(p.images(p.supportMax.fold(0)(_ + 1))) )
}
