package com.faacets.perm

import org.scalacheck._
import com.faacets.perm._

object PermutationGenerators {
  val genPermutation = for {
    k <- Gen.choose(1, 20)
  } yield Permutation.random(k)
  val genPermutationPair = for {
    k <- Gen.choose(1, 20)
  } yield (Permutation.random(k), Permutation.random(k))
  val genIdPermutation = for {
    k <- Gen.choose(1, 20)
  } yield Permutation((0 until k):_*)
}

object PermutationSpecification extends Properties("Permutation") {
  import PermutationGenerators._

  property("isIdentity") = Prop.forAll(genPermutation) { (pp: Permutation) => (pp*pp.inverse).isIdentity }
  property("inverse") = Prop.forAll(genPermutation) { (pp: Permutation) => pp.equals(pp.inverse.inverse) }
  property("equal") = Prop.forAll(genPermutation) { (pp: Permutation) => pp.equals(pp) }
  property("*") = Prop.forAll(genPermutationPair) { case ((p1: Permutation, p2: Permutation)) => ((p1*p2).inverse).equals((p2.inverse)*(p1.inverse)) }
  property("image") = Prop.forAll(genPermutation) { (pp: Permutation) => (0 until pp.domainSize).forall(i => pp.inverse.image(pp.image(i)) == i) }
}
