package com.faacets
package perm
package bsgs

import org.scalacheck._
import com.faacets.perm._
import com.faacets.perm.bsgs._
import scala.util.Random

object BSGSGenerators {
  implicit val random = Random
  val genBSGS = for {
    degree <- Gen.choose(1, 15)
    s = Sym(degree)
    bsgs = BSGS.randomSchreierSims(FullBase, ExplicitTransversalFactory[Perm]())(s.random, s.identity, s.order)
  } yield (s, bsgs)

  val genBSGSAndElement1 = for {
    (s, bsgs) <- genBSGS
    se = s.random
    be = bsgs.fromExplicit(se).get
  } yield (s, bsgs, se, be)
  val genBSGSAndElement2 = for {
    (s, bsgs) <- genBSGS
    se1 = s.random
    se2 = s.random
    be1 = bsgs.fromExplicit(se1).get
    be2 = bsgs.fromExplicit(se2).get
  } yield (s, bsgs, se1, se2, be1, be2)

}

object BSGSSpecification extends Properties("BSGS") {
  import BSGSGenerators._
  property("fromExplicit/image") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => be.image(b.trv.beta) == be.b && se.image(b.trv.beta) == be.b
  ) }

  property("equal/explicit") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => be.explicit.equal(se)
  ) }
  property("inverse/equal/explicit") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => be.inverse.explicit.equal(se.inverse)
  ) }
  property("*/fromExplicit/explicit") = Prop.forAll(genBSGSAndElement2) { 
   case (s: Sym, b: BSGSGroup[Perm], se1: Perm, se2: Perm, be1: BSGSElement[Perm], be2: BSGSElement[Perm]) => (be1*be2).explicit.equal(se1*se2)
  }
}
