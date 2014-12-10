package net.alasc.laws

import spire.algebra._
import spire.algebra.lattice._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import spire.syntax.all._
import spire.laws._
import spire.std.boolean._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._

object GroupoidLaws {
  def apply[A <: AnyRef : Eq : Arbitrary] = new GroupLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait GroupoidLaws[A <: AnyRef] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  implicit def RefOptionEqu: Eq[RefOption[A]] = new Eq[RefOption[A]] {
    def eqv(aOpt: RefOption[A], bOpt: RefOption[A]): Boolean = aOpt match {
      case RefOption(a) => bOpt match {
        case RefOption(b) => a === b
        case _ => false
      }
      case _ => bOpt.isEmpty
    }
  }

  def semigroupoid(implicit A: Semigroupoid[A]) = new GroupoidProperties(
    name = "semigroupoid",
    parent = None,
    "associative: a ?+? b && b ?+? c imply (a |+|! b) ?+? c" → forAll((a: A, b: A, c: A) =>
      !((a ?+? b) && (b ?+? c)) || ((a |+|! b) ?+? c)
    ),

    "associative: (a |+|? b) |+|? c === a |+|? (b |+|? c)" → forAll((a: A, b: A, c: A) => {
      import A.RefOptionSemigroup

      ((RefOption(a) |+| RefOption(b)) |+| RefOption(c)) === (RefOption(a) |+| (RefOption(b) |+| RefOption(c)))
    }
    )
  )

  def partialMonoid(implicit A: PartialMonoid[A]) = new GroupoidProperties(
    name = "partialMonoid",
    parent = Some(semigroupoid),

    "left identity" → forAll((a: A) =>
      (a.leftId ?+? a) && ((a.leftId() |+|! a) === a)
    ),

    "right identity" → forAll((a: A) =>
      (a ?+? a.rightId) && ((a |+|! a.rightId) === a)
    )
  )

  def groupoid(implicit A: Groupoid[A]) = new GroupoidProperties(
    name = "groupoid",
    parent = Some(partialMonoid),

    "product with inverse is always defined" → forAll((a: A) =>
      (a ?+? a.inverse) && (a.inverse ?+? a)
    ),

    "product with inverse is a left and right identity" → forAll((a: A, b: A) =>
      !(a ?+? b) || (
        (a |+|! b |+|! b.inverse) === a &&
          (a.inverse |+|! a |+|! b) === b
      )
    )
  )

  class GroupoidProperties(
    name: String,
    parent: Option[GroupoidProperties],
    props: (String, Prop)*
  ) extends DefaultRuleSet(name, parent, props: _*)
}
