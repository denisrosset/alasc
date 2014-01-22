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
  def fact(n: Int): BigInt = n match {
    case 0 => 1
    case 1 => 1
    case _ => fact(n - 1) * n
  }
  property("order") = Prop.forAll(genSym) { s => s.order == fact(s.degree) }
  property("contains/identity") = Prop.forAll(genSym) { s: Sym => s.contains(s.identity) }
}
