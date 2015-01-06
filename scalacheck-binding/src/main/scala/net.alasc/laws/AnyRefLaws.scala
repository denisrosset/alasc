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

trait TwoInstances[A] extends Any {
  def first: A
  def second: A
}

object TwoInstances {
  def apply[A](implicit ev: TwoInstances[A]): TwoInstances[A] = ev
}

trait Cloner[A] extends Any {
  def makeClone(a: A): A
}

object Cloner {
  def apply[A](implicit ev: Cloner[A]): Cloner[A] = ev
}

object AnyRefLaws {
  def apply[A : Eq : Arbitrary : TwoInstances : Cloner] = new AnyRefLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def Clnr = Cloner[A]
    def TwoI = TwoInstances[A]
  }
}

trait AnyRefLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]
  implicit def Clnr: Cloner[A]
  implicit def TwoI: TwoInstances[A]

  def _equals = new AnyRefProperties(
    "equals",
    parents = Nil,

    "Clone equals original" → forAll((a: A) =>
      Clnr.makeClone(a) == a
    ),

    "two instances are different" →
      Prop(TwoI.first != TwoI.second),

    ".equals compatible with Eq" → forAll((x: A, y: A) =>
      (x === y) == (x == y)
    )
  )

  def _hashCode = new AnyRefProperties(
    "hash",
    parents = Nil,

    "two instances have different hashCodes" →
      Prop(TwoI.first.hashCode =!= TwoI.second.hashCode),

    "clone has same hashCode" → forAll((a: A) =>
      Clnr.makeClone(a).hashCode === a.hashCode
    ),

    "(x === y) (x.hashCode === y.hashCode)" → forAll((x: A, y: A) =>
      (x === y) imp (x.hashCode === y.hashCode)
    )
  )

  def _anyRef = new AnyRefProperties(
    "anyRef",
    parents = Seq(_hashCode, _equals)
  )

  class AnyRefProperties(
    val name: String,
    val parents: Seq[AnyRefProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    def bases = Seq.empty[(String, Laws#RuleSet)]
  }
}
