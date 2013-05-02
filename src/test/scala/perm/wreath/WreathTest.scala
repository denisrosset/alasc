package com.faacets
package perm
package wreath

import org.scalacheck._
import com.faacets.perm._
import scala.util.Random

object WreathGroupGenerators {
  implicit val r = Random
  val genSmallWreathGroup = for {
    n <- Gen.choose(1, 3)
    m <- Gen.choose(1, 3)
  } yield new WreathGroup[Sym, Perm, Sym, Perm](Sym(m), Sym(n))

  val genWreathGroup = for {
    n <- Gen.choose(1, 6)
    m <- Gen.choose(1, 6)
  } yield new WreathGroup[Sym, Perm, Sym, Perm](Sym(m), Sym(n))

  val genWreathGroupAndElement = for {
    w <- genWreathGroup
  } yield (w, w.random)

  val genWreathGroupAndTwoElements = for {
    w <- genWreathGroup
  } yield (w, w.random, w.random)

  val genSmallWreathGroupAndElement = for {
    w <- genSmallWreathGroup
  } yield (w, w.random)

  val genSmallWreathGroupAndTwoElements = for {
    w <- genSmallWreathGroup
  } yield (w, w.random, w.random)

}

object WreathGroupSpecification extends Properties("WreathGroup") {
  import WreathGroupGenerators._
  type WG = WreathGroup[Sym, Perm, Sym, Perm]
  type WEG = WreathElement[Perm, Perm]
  property("inverse/equal") = Prop.forAll(genWreathGroupAndElement) { Function.tupled(
    (w, we) => we.inverse.inverse.equal(we)
  ) }
  property("*/inverse/equal") = Prop.forAll(genWreathGroupAndTwoElements) { Function.tupled(
    (w, we1, we2) => (we1*we2).inverse.equal(we2.inverse*(we1.inverse))
  ) }
}
