package net.alasc.laws

/*import spire.algebra._
import spire.algebra.lattice._
import spire.implicits._
 */

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import spire.algebra.Eq
import spire.std.int._
import spire.std.boolean._
import spire.syntax.eq._
import spire.syntax.bool._

object HashLaws {
  def apply[A : Eq : Arbitrary](el1: A, el2: A, makeCloneFun: A => A) = new HashLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def makeClone(a: A) = makeCloneFun(a)
    assert(el1.hashCode =!= el2.hashCode)
  }
}

trait HashLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def makeClone(a: A): A

  def hash = new SimpleRuleSet(
    "hash",

    "clone has same hashCode" → forAll((a: A) =>
      makeClone(a).hashCode === a.hashCode
    ),

    "equal.hashCode" → forAll((x: A, y: A) =>
      (x === y) imp (x.hashCode === y.hashCode)
    )
  )
}
