package com.faacets
package perm
package wreath

import org.scalacheck._
import com.faacets.perm._
import scala.util.Random

object InhWreathGroupGenerators {
  implicit val r = Random

  def leaveInvariant[E <: PermElement[E], D](s: Seq[D])(e: E) =
    s.sameElements(s.indices.map(i => s(e.image(Domain.zeroBased(i)).zeroBased)))

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
  property("InhPrimitiveAction/inverse/equal") = Prop.forAll(genSmallInhWreathGroupAndElement) { Function.tupled(
    (w, we) => {
      val ba1 = TrivialAction[Perm]()
      val ba = w.a.map(i => ba1.asInstanceOf[Action[Perm, Perm]])
     val a = new InhPrimitiveAction[WG, WEG, WG, WEG, Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](ba, w.a.map(_.degree))
      a(we).inverse.equal(a(we.inverse))
    }
  ) }
  property("InhImprimitiveAction/inverse/equal") = Prop.forAll(genSmallInhWreathGroupAndElement) { Function.tupled(
    (w, we) => {
      val ba1 = TrivialAction[Perm]()
      val ba = w.a.map(i => ba1.asInstanceOf[Action[Perm, Perm]])
     val a = new InhImprimitiveAction[WG, WEG, WG, WEG, Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](ba, w.a.map(_.degree))
      a(we).inverse.equal(a(we.inverse))
    }
  ) }
  property("InhPrimitiveAction / *") = Prop.forAll(genSmallInhWreathGroupAndTwoElements) { Function.tupled(
    (w, we1, we2) => {
      val ba1 = TrivialAction[Perm]()
      val ba = w.a.map(i => ba1.asInstanceOf[Action[Perm, Perm]])
     val a = new InhPrimitiveAction[WG, WEG, WG, WEG, Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](ba, w.a.map(_.degree))
      a(we1*we2).equal(a(we1)*a(we2))
    }
  ) }
  property("InhImprimitiveAction / *") = Prop.forAll(genSmallInhWreathGroupAndTwoElements) { Function.tupled(
    (w, we1, we2) => {
      val ba1 = TrivialAction[Perm]()
      val ba = w.a.map(i => ba1.asInstanceOf[Action[Perm, Perm]])
     val a = new InhImprimitiveAction[WG, WEG, WG, WEG, Sym, Perm, PredicateSubgroup[Sym, Perm], Perm](ba, w.a.map(_.degree))
      a(we1*we2).equal(a(we1)*a(we2))
    }
  ) }
}
