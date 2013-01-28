package com.faacets.perm

import org.scalatest.FunSuite
import org.scalacheck._
import com.faacets.perm._

class ExplicitTransversalSuite extends FunSuite {
  test("Example 2.6 of Thomas Rehn diploma thesis") {
    val a = ExplicitPermutation(5)(0,1,4)
    val b = ExplicitPermutation(5)(0,3)(2,4)
    val t = ExplicitTransversal.fromGenerators(0, List(a, b))
    assert(t(0) === ExplicitPermutation(5))
    assert(t(1) === ExplicitPermutation(5)(0,4,1))
    assert(t(2) === ExplicitPermutation(5)(0,3,1,4,2))
    assert(t(3) === ExplicitPermutation(5)(0,3)(2,4))
    assert(t(4) === ExplicitPermutation(5)(0,1,4))
  }
}

object ExplicitTransversalSpecification extends Properties("ExplicitTransversal") {
  val genElementGenerators = for {
    k <- Gen.choose(3, 20)
    n <- Gen.choose(2, 5)
    el <- Gen.choose(0, k - 1)
  } yield (el, (0 until n).map(a => ExplicitPermutation.random(k)))
  property("contains") = Prop.forAll(genElementGenerators) {
    case ((el: Domain, gens: Iterable[_])) => {
      val t = ExplicitTransversal.fromGenerators(el, gens:Iterable[ExplicitPermutation])
      t.contains(el) && (for (e <- t.iterable; g <- gens:Iterable[ExplicitPermutation]) yield t.contains(g.image(e)) && t.contains(g.inverse.image(e))).forall((b:Boolean) => b)
    }
  }

  property("apply") = Prop.forAll(genElementGenerators) {
    case ((el: Domain, gens: Iterable[_])) => {
      val t = ExplicitTransversal.fromGenerators(el, gens:Iterable[ExplicitPermutation])
      t.iterable.forall(e => t(e).image(e) == el)
    }
  }
}
