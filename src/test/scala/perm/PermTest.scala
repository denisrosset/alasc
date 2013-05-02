package com.faacets
package perm

import org.scalacheck._
import com.faacets.perm._

object PermGenerators {
  implicit val r = scala.util.Random
  val genPerm = for {
    k <- Gen.choose(1, 20)
  } yield Sym(k).random
  val genPermPair = for {
    k <- Gen.choose(1, 20)
  } yield (Sym(k).random, Sym(k).random)
  val genIdPermutation = for {
    k <- Gen.choose(1, 20)
  } yield Perm(k)
}

object PermSpecification extends Properties("Perm") {
  import PermGenerators._

  property("*/inverse/isIdentity") = Prop.forAll(genPerm) {
    pp => (pp*pp.inverse).isIdentity
  }
/*
  property("inverse") = Prop.forAll(genPermutation) { (pp: Permutation) => pp.equals(pp.inverse.inverse) }
  property("equal") = Prop.forAll(genPermutation) { (pp: Permutation) => pp.equals(pp) }
  property("*") = Prop.forAll(genPermutationPair) { case ((p1: Permutation, p2: Permutation)) => ((p1*p2).inverse).equals((p2.inverse)*(p1.inverse)) }
  property("image") = Prop.forAll(genPermutation) { (pp: Permutation) => (0 until pp.domainSize).forall(i => pp.inverse.image(pp.image(i)) == i) }
 */
}
