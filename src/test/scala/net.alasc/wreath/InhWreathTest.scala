package net.alasc
package wreath

import org.scalacheck._
import scala.util.Random

object InhWreathGroupGenerators {
  implicit val r = Random

  val genGroupS = for {
    n <- Gen.choose(1, 3)
    marr <- Gen.containerOfN[Array, Int](n, Gen.choose(1, 3))
    h = new PredicateSubgroup[Sym, Perm](Sym(n), InvariantPredicate(marr))
  } yield new InhWreathGroup[InhWreathElement[Perm, Perm], Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](marr.map(Sym(_)), h, InhWreathElement.apply[Perm, Perm])

  val genGroupB = for {
    n <- Gen.choose(1, 6)
    marr <- Gen.containerOfN[Array, Int](n, Gen.choose(1, 6))
    h = new PredicateSubgroup[Sym, Perm](Sym(n), InvariantPredicate(marr))
  } yield new InhWreathGroup[InhWreathElement[Perm, Perm], Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](marr.map(Sym(_)), h, InhWreathElement.apply[Perm, Perm])

  val genElement1S = for { w <- genGroupS } yield (w, w.random)

  val genElement1B = for { w <- genGroupB } yield (w, w.random)

  val genElement2S = for { w <- genGroupS } yield (w, w.random, w.random)

  val genElement2B = for { w <- genGroupB } yield (w, w.random, w.random)

  val genPrimitive1 = for {
    (w, we) <- genElement1S
    ba = w.a.map(a => TrivialAction(a.identity).asInstanceOf[Action[Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm,Perm], Perm, Perm](ba, w.identity)
  } yield (w, we, a)

  val genPrimitive2 = for {
    (w, we1, we2) <- genElement2S
    ba = w.a.map(a => TrivialAction(a.identity).asInstanceOf[Action[Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm,Perm], Perm, Perm](ba, w.identity)
  } yield (w, we1, we2, a)

  val genImprimitive1 = for {
    (w, we) <- genElement1S
    ba = w.a.map(a => TrivialAction(a.identity).asInstanceOf[Action[Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm,Perm], Perm, Perm](ba, w.identity)
  } yield (w, we, a)

  val genImprimitive2 = for {
    (w, we1, we2) <- genElement2S
    ba = w.a.map(a => TrivialAction(a.identity).asInstanceOf[Action[Perm]])
    a = new InhPrimitiveAction[InhWreathElement[Perm,Perm], Perm, Perm](ba, w.identity)
  } yield (w, we1, we2, a)

  val genPrimitive1D = for {
    (w, we, a) <- genPrimitive1
    k <- Gen.choose(0, a.dimension - 1)
  } yield (w, we, a, Dom._0(k))

  val genImprimitive1D = for {
    (w, we, a) <- genImprimitive1
    k <- Gen.choose(0, a.dimension - 1)
  } yield (w, we, a, Dom._0(k))

  val genPrimitive2D = for {
    (w, we1, we2, a) <- genPrimitive2
    k <- Gen.choose(0, a.dimension - 1)
  } yield (w, we1, we2, a, Dom._0(k))

  val genImprimitive2D = for {
    (w, we1, we2, a) <- genImprimitive2
    k <- Gen.choose(0, a.dimension - 1)
  } yield (w, we1, we2, a, Dom._0(k))

}

object InhWreathGroupSpecification extends Properties("InhWreathGroup") {
  import InhWreathGroupGenerators._
  type WG = InhWreathGroup[InhWreathElement[Perm, Perm], Sym, Perm, PredicateSubgroup[Sym, Perm], Perm]
  type WEG = InhWreathElement[Perm, Perm]
  property("inverse/===") = Prop.forAll(genElement1B) { Function.tupled(
    (w, we) => we.inverse.inverse === we
  ) }

  property("* / inverse / ===") = Prop.forAll(genElement2B) { Function.tupled(
    (w, we1, we2) => (we1*we2).inverse === (we2.inverse*(we1.inverse))
  ) }

  property("InhPrimitiveAction/inverse/===") = Prop.forAll(genPrimitive1) { Function.tupled(
    (w, we, a) => a.toPerm(we).inverse === (a.toPerm(we.inverse))
  ) }

  property("InhImprimitiveAction/inverse/===") = Prop.forAll(genImprimitive1) { Function.tupled(
    (w, we, a) => a.toPerm(we).inverse === (a.toPerm(we.inverse))
  ) }

  property("InhPrimitiveAction/inverse/image") = Prop.forAll(genPrimitive1D) { Function.tupled(
    (w, we, a, k) => a.toPerm(we).inverse.image(k) === a(we.inverse, k)
  ) }
  property("InhImprimitiveAction/inverse/image") = Prop.forAll(genImprimitive1D) { Function.tupled(
    (w, we, a, k) => a.toPerm(we).inverse.image(k) === a(we.inverse, k)
  ) }

  property("InhPrimitiveAction / *") = Prop.forAll(genPrimitive2) { Function.tupled(
    (w, we1, we2, a) => a.toPerm(we1*we2) === (a.toPerm(we1)*a.toPerm(we2))
  ) }
  property("InhImprimitiveAction / *") = Prop.forAll(genImprimitive2) { Function.tupled(
    (w, we1, we2, a) => a.toPerm(we1*we2) === (a.toPerm(we1)*a.toPerm(we2))
  ) }

  property("InhPrimitiveAction / * / image") = Prop.forAll(genPrimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => a(we2, a(we1, k)) === a.toPerm(we1*we2).image(k)
  ) }

  property("InhImprimitiveAction / * / image") = Prop.forAll(genImprimitive2D) { Function.tupled(
    (w, we1, we2, a, k) => a(we2, a(we1, k)) === a.toPerm(we1*we2).image(k)
  ) }
}
