package net.alasc.laws

import scala.collection.immutable.BitSet

object BitSets {
/*
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


  def sized: Gen[BitSet] = Gen.parameterized { parameters =>

  }

  def sized[P:PermutationBuilder]: Gen[] = Gen.parameterized { parameters =>
    val images = randomImages(parameters.size, parameters.rng)
    PermutationBuilder[P].fromImages(images)
  }

  def forSize[P:PermutationBuilder](domainSize: Int) = Gen.parameterized { parameters =>
    val images = randomImages(domainSize, parameters.rng)
    PermutationBuilder[P].fromImages(images)
  }
  implicit def arbPermutation[P:PermutationBuilder]: Arbitrary[P] = Arbitrary(sized[P])
*/

}
