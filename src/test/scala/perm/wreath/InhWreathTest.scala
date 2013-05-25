package com.faacets
package perm
package wreath

import org.scalacheck._
import com.faacets.perm._
import scala.util.Random

object InhWreathGroupGenerators {
  implicit val r = Random

  val genGroupS = for {
    n <- Gen.choose(1, 3)
    marr <- Gen.containerOfN[Array, Int](n, Gen.choose(1, 3))
    h = new PredicateSubgroup[Sym, Perm](Sym(n), InvariantPredicate(marr))
  } yield new InhWreathGroup[Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](marr.map(Sym(_)), h)

  val genGroupB = for {
    n <- Gen.choose(1, 6)
    marr <- Gen.containerOfN[Array, Int](n, Gen.choose(1, 6))
    h = new PredicateSubgroup[Sym, Perm](Sym(n), InvariantPredicate(marr))
  } yield new InhWreathGroup[Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](marr.map(Sym(_)), h)

  val genElement1S = for { w <- genGroupS } yield (w, w.random)

  val genElement1B = for { w <- genGroupB } yield (w, w.random)

  val genElement2S = for { w <- genGroupS } yield (w, w.random, w.random)

  val genElement2B = for { w <- genGroupB } yield (w, w.random, w.random)

  val genPrimitive1 = for {
    (w, we) <- genElement1S
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
  } yield (w, we, a)

  val genPrimitive2 = for {
    (w, we1, we2) <- genElement2S
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
  } yield (w, we1, we2, a)

  val genImprimitive1 = for {
    (w, we) <- genElement1S
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhImprimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
  } yield (w, we, a)

  val genImprimitive2 = for {
    (w, we1, we2) <- genElement2S
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhImprimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
  } yield (w, we1, we2, a)

  val genPrimitive1D = for {
    (w, we, a) <- genPrimitive1
    k <- Gen.choose(0, a(we).size - 1)
  } yield (w, we, a, Dom._0(k))

  val genImprimitive1D = for {
    (w, we, a) <- genImprimitive1
    k <- Gen.choose(0, a(we).size - 1)
  } yield (w, we, a, Dom._0(k))

  val genPrimitive2D = for {
    (w, we1, we2, a) <- genPrimitive2
    k <- Gen.choose(0, a(we1).size - 1)
  } yield (w, we1, we2, a, Dom._0(k))

  val genImprimitive2D = for {
    (w, we1, we2, a) <- genImprimitive2
    k <- Gen.choose(0, a(we1).size - 1)
  } yield (w, we1, we2, a, Dom._0(k))

}

object InhWreathGroupSpecification extends Properties("InhWreathGroup") {
  import InhWreathGroupGenerators._
  type WG = InhWreathGroup[Sym, Perm, PredicateSubgroup[Sym, Perm], Perm]
  type WEG = InhWreathElement[Perm, Perm]
  property("inverse/equal") = Prop.forAll(genElement1B) { Function.tupled(
    (w, we) => we.inverse.inverse.equal(we)
  ) }

  property("* / inverse / equal") = Prop.forAll(genElement2B) { Function.tupled(
    (w, we1, we2) => (we1*we2).inverse.equal(we2.inverse*(we1.inverse))
  ) }

  property("InhPrimitiveAction/inverse/equal") = Prop.forAll(genPrimitive1) { Function.tupled(
    (w, we, a) => a(we).inverse.equal(a(we.inverse))
  ) }

  property("InhImprimitiveAction/inverse/equal") = Prop.forAll(genImprimitive1) { Function.tupled(
    (w, we, a) => a(we).inverse.equal(a(we.inverse))
  ) }

  property("InhPrimitiveAction/inverse/explicit") = Prop.forAll(genPrimitive1) { Function.tupled(
    (w, we, a) => a(we).inverse.explicit.equal(a(we.inverse).explicit)
  ) }
  property("InhImprimitiveAction/inverse/explicit") = Prop.forAll(genImprimitive1) { Function.tupled(
    (w, we, a) => a(we).inverse.explicit.equal(a(we.inverse).explicit)
  ) }

  property("InhPrimitiveAction/inverse/image") = Prop.forAll(genPrimitive1D) { Function.tupled(
    (w, we, a, k) => a(we).inverse.image(k) == a(we.inverse).image(k)
  ) }
  property("InhImprimitiveAction/inverse/image") = Prop.forAll(genImprimitive1D) { Function.tupled(
    (w, we, a, k) => a(we).inverse.image(k) == a(we.inverse).image(k)
  ) }

  property("InhPrimitiveAction/inverse/invImage") = Prop.forAll(genPrimitive1D) { Function.tupled(
    (w, we, a, k) => a(we.inverse).image(k) == a(we).invImage(k)
  ) }
  property("InhImprimitiveAction/inverse/invImage") = Prop.forAll(genImprimitive1D) { Function.tupled(
    (w, we, a, k) => a(we.inverse).image(k) == a(we).invImage(k)
  ) }

  property("InhPrimitiveAction / *") = Prop.forAll(genPrimitive2) { Function.tupled(
    (w, we1, we2, a) => a(we1*we2).equal(a(we1)*a(we2))
  ) }
  property("InhImprimitiveAction / *") = Prop.forAll(genImprimitive2) { Function.tupled(
    (w, we1, we2, a) => a(we1*we2).equal(a(we1)*a(we2))
  ) }

  property("InhPrimitiveAction / * / explicit") = Prop.forAll(genPrimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => (a(we1).explicit*(a(we2).explicit)).equal(a(we1*we2).explicit)
  ) }
  property("InhImprimitiveAction / * / explicit") = Prop.forAll(genImprimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => (a(we1).explicit*(a(we2).explicit)).equal(a(we1*we2).explicit)
  ) }

  property("InhPrimitiveAction / * / inverse / explicit") = Prop.forAll(genPrimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => (a(we2).explicit.inverse*(a(we1).explicit.inverse)).equal(a((we1*we2).inverse).explicit)
  ) }
  property("InhImprimitiveAction / * / inverse / explicit") = Prop.forAll(genImprimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => (a(we2).explicit.inverse*(a(we1).explicit.inverse)).equal(a((we1*we2).inverse).explicit)
  ) }

  property("InhPrimitiveAction / * / image") = Prop.forAll(genPrimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => a(we2).image(a(we1).image(k)) == a(we1*we2).image(k)
  ) }

  property("InhImprimitiveAction / * / image") = Prop.forAll(genImprimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => a(we2).image(a(we1).image(k)) == a(we1*we2).image(k)
  ) }
}
