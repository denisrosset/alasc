package com.faacets.perm

import org.scalacheck._
import com.faacets.perm._
import com.faacets.bell._
object ExplicitPermutationGenerators {
  val genPermutation = for {
    k <- Gen.choose(1, 20)
  } yield ExplicitPermutation.random(k)
  val genPermutationPair = for {
    k <- Gen.choose(1, 20)
  } yield (ExplicitPermutation.random(k), ExplicitPermutation.random(k))
  val genIdPermutation = for {
    k <- Gen.choose(1, 20)
  } yield new ExplicitPermutation(Vector((0 until k):_*))
}
object ExplicitPermutationSpecification extends Properties("ExplicitPermutation") {
  import ExplicitPermutationGenerators._

  property("isIdentity") = Prop.forAll(genIdPermutation) { (pp: ExplicitPermutation) => pp.isIdentity }
  property("identity") = Prop.forAll(genPermutation) { (pp: ExplicitPermutation) => pp.identity.isIdentity }
  property("inverse") = Prop.forAll(genPermutation) { (pp: ExplicitPermutation) => (pp*pp.inverse).isIdentity }
  property("inverse") = Prop.forAll(genPermutation) { (pp: ExplicitPermutation) => pp.equal(pp.inverse.inverse) }
  property("equal") = Prop.forAll(genPermutation) { (pp: ExplicitPermutation) => pp.equal(pp) }
  property("*") = Prop.forAll(genPermutationPair) { case ((p1: ExplicitPermutation, p2: ExplicitPermutation)) => ((p1*p2).inverse).equal((p2.inverse)*(p1.inverse)) }
  property("image") = Prop.forAll(genPermutation) { (pp: ExplicitPermutation) => (0 until pp.domainSize).forall(i => pp.inverse.image(pp.image(i)) == i) }
}
