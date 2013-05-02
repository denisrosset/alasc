package com.faacets
package perm

import org.scalacheck._
import com.faacets.perm._
import scala.util.Random

object SymmetricGroupGenerators {
  implicit val random = Random
  val genSymmetricGroup = for {
    degree <- Gen.choose(1, 20)
  } yield SymmetricGroup(degree)

  val genSymmetricGroupAndElement = for {
    s <- genSymmetricGroup
    e = s.full.randomElement
  } yield (s, e)

  val genSymmetricGroupAndTwoElements = for {
    s <- genSymmetricGroup
    e1 = s.full.randomElement
    e2 = s.full.randomElement
  } yield (s, e1, e2)
}

object SymmetricGroupSpecification extends Properties("SymmetricGroup") {
  import SymmetricGroupGenerators._
  type SG = SymmetricGroup
  type SE = SymmetricGroup#SymmetricElement
  property("identity") = Prop.forAll(genSymmetricGroup) { s: SymmetricGroup => s.full.contains(s.identity) }
  property("fromRaw/equal") = Prop.forAll(genSymmetricGroupAndElement) { Function.tupled(
    (s, e) => s.fromRaw(e.raw).get.equal(e) 
  ) }
  property("Element.*") = Prop.forAll(genSymmetricGroupAndTwoElements) { Function.tupled(
    (s: SG, e1: SE, e2: SE) => (e1*e2).raw == (e1.raw)*(e2.raw)
  ) }
  property("Element.equal") = Prop.forAll(genSymmetricGroupAndTwoElements) { Function.tupled(
    (s: SG, e1: SE, e2: SE) => (e1*e2).raw == (e1.raw)*(e2.raw)
  ) }
}
