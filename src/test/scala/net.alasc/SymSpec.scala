package net.alasc

import org.scalacheck._
import scala.util.Random

object SymGenerators {
  implicit val random = Random
  val genSym = for {
    degree <- Gen.choose(1, 20)
  } yield Sym(degree)

  val genSymAndElement = for {
    s <- genSym
  } yield (s, s.random)

  val genSymAndTwoElements = for {
    s <- genSym
  } yield (s, s.random, s.random)
}

object SymSpec extends Properties("Sym") {
  import SymGenerators._
  property("contains/identity") = Prop.forAll(genSym) { s: Sym => s.contains(s.identity) }
  property("fromExplicit/equal") = Prop.forAll(genSymAndElement) {
    case (s, e) => s.fromExplicit(e.explicit).get === e
  }
}
