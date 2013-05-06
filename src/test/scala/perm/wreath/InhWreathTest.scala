package com.faacets
package perm
package wreath

import org.scalacheck._
import com.faacets.perm._
import scala.util.Random

object InhWreathGroupGenerators {
  implicit val r = Random

  val genSmallInhWreathGroup = for {
    n <- Gen.choose(1, 3)
    marr <- Gen.containerOfN[Array, Int](n, Gen.choose(1, 3))
    h = new PredicateSubgroup[Sym, Perm](Sym(n), leaveInvariant(marr))
  } yield new InhWreathGroup[Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](marr.map(Sym(_)), h)

  val genInhWreathGroup = for {
    n <- Gen.choose(1, 6)
    marr <- Gen.containerOfN[Array, Int](n, Gen.choose(1, 6))
    h = new PredicateSubgroup[Sym, Perm](Sym(n), leaveInvariant(marr))
  } yield new InhWreathGroup[Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](marr.map(Sym(_)), h)

  val genInhWreathGroupAndElement = for {
    w <- genInhWreathGroup
  } yield (w, w.random)

  val genInhWreathGroupAndTwoElements = for {
    w <- genInhWreathGroup
  } yield (w, w.random, w.random)

  val genSmallInhWreathGroupAndElement = for {
    w <- genSmallInhWreathGroup
  } yield (w, w.random)

  val genSmallInhWreathGroupAndTwoElements = for {
    w <- genSmallInhWreathGroup
  } yield (w, w.random, w.random)

}

object InhWreathGroupSpecification extends Properties("InhWreathGroup") {
  import InhWreathGroupGenerators._
  type WG = InhWreathGroup[Sym, Perm, PredicateSubgroup[Sym, Perm], Perm]
  type WEG = InhWreathElement[Perm, Perm]
  property("inverse/equal") = Prop.forAll(genInhWreathGroupAndElement) { Function.tupled(
    (w, we) => we.inverse.inverse.equal(we)
  ) }
  property("*/inverse/equal") = Prop.forAll(genInhWreathGroupAndTwoElements) { Function.tupled(
    (w, we1, we2) => (we1*we2).inverse.equal(we2.inverse*(we1.inverse))
  ) }

  val genPrimitive1 = for {
    (w, we) <- genSmallInhWreathGroupAndElement
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
  } yield (w, we, a)

  val genPrimitive2 = for {
    (w, we1, we2) <- genSmallInhWreathGroupAndTwoElements
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
  } yield (w, we1, we2, a)

  val genPrimitive2D = for {
    (w, we1, we2) <- genSmallInhWreathGroupAndTwoElements
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
    k <- Gen.choose(0, a(we1).size - 1)
  } yield (w, we1, we2, a, Dom._0(k))

  val genImprimitive1 = for {
    (w, we) <- genSmallInhWreathGroupAndElement
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhImprimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
  } yield (w, we, a)

  val genImprimitive2 = for {
    (w, we1, we2) <- genSmallInhWreathGroupAndTwoElements
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhImprimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
  } yield (w, we1, we2, a)

  val genImprimitive2D = for {
    (w, we1, we2) <- genSmallInhWreathGroupAndTwoElements
    ba = w.a.map(a => TrivialAction[Perm]().asInstanceOf[Action[Perm, Perm]])
    a = new InhImprimitiveAction[InhWreathElement[Perm, Perm], Perm, Perm](ba)
    k <- Gen.choose(0, a(we1).size - 1)
  } yield (w, we1, we2, a, Dom._0(k))

  property("InhPrimitiveAction/inverse/equal") = Prop.forAll(genPrimitive1) { Function.tupled(
    (w, we, a) => a(we).inverse.equal(a(we.inverse))
  ) }
  property("InhImprimitiveAction/inverse/equal") = Prop.forAll(genImprimitive1) { Function.tupled(
    (w, we, a) => a(we).inverse.equal(a(we.inverse))
  ) }
  property("InhPrimitiveAction / *") = Prop.forAll(genPrimitive2) { Function.tupled(
    (w, we1, we2, a) => a(we1*we2).equal(a(we1)*a(we2))
  ) }
  property("InhPrimitiveAction / *") = Prop.forAll(genPrimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => a(we2).image(a(we1).image(k)) == a(we1*we2).image(k)
  ) }

  property("InhImprimitiveAction / *") = Prop.forAll(genImprimitive2) { Function.tupled(
    (w, we1, we2, a) => a(we1*we2).equal(a(we1)*a(we2))
  ) }

  property("InhImprimitiveAction / *") = Prop.forAll(genImprimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => a(we2).image(a(we1).image(k)) == a(we1*we2).image(k)
  ) }
}
