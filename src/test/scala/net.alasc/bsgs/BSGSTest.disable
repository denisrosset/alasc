package net.alasc
package bsgs

import org.scalacheck._
import scala.util.Random

object BSGSGenerators {
  implicit val random = Random
  val genBSGS = for {
    degree <- Gen.choose(1, 15)
    s = Sym(degree)
    bsgs = BSGS.randomSchreierSims(s.random, s.order, s.identity)
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
  property("transversalElement") = Prop.forAll(genBSGS) { Function.tupled(
    (s, b) => b.transversal.keysIterator.forall( k => b.transversalElement(0, k).represents === (b.transversal.u(k)) )
  ) }
  property("fromSequence/sequence") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => b.fromSequence(be.sequence) === (be) 
  ) }

  property("fromBaseImage/baseImage") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => b.fromBaseImage(be.baseImage) === (be) 
  ) }

  property("fromExplicit/image") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => be.image(b.transversal.beta) == be.b && se.image(b.transversal.beta) == be.b
  ) }

  property("fromExplicit/invImage") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => be.invImage(be.b) == b.transversal.beta
  ) }

  property("===/explicit") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => be.explicit === se
  ) }
  property("inverse/===/explicit") = Prop.forAll(genBSGSAndElement1) { Function.tupled(
    (s, b, se, be) => be.inverse.explicit === (se.inverse)
  ) }
  property("*/fromExplicit/explicit") = Prop.forAll(genBSGSAndElement2) { 
   case (s: Sym, b: BSGSGroup[Perm], se1: Perm, se2: Perm, be1: BSGSElement[Perm], be2: BSGSElement[Perm]) => (be1*be2).explicit === (se1*se2)
  }
}
