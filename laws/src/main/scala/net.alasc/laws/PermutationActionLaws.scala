package net.alasc.laws

import spire.algebra._

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

object PermutationActionLaws {
  def apply[A : Eq : Arbitrary](implicit da: Arbitrary[Dom])  = new PermutationActionLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def DomArb = da
  }
}

trait PermutationActionLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]
  implicit def DomArb: Arbitrary[Dom]

  def permutationAction(implicit group: Group[A], A: PermutationAction[A]) = new PermutationActionProperties(
    name = "permutationAction",
    parent = None,
    bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> ActionLaws[A, Dom].groupAction),

    "supportMin" → forAll((x: A) =>
      x.smallestMovedPoint match {
        case NNOption(sm) => x.movedPoints.min === sm && ((sm <|+| x) != sm)
        case _ => x.movedPoints.isEmpty
      }
    ),

    "supportMax" → forAll((x: A) =>
      x.largestMovedPoint match {
        case NNOption(sm) => x.movedPoints.max === sm && ((sm <|+| x) != sm)
        case _ => x.movedPoints.isEmpty
      }
    ),

    "supportAny" → forAll((x: A) =>
      x.findMovedPoint match {
        case NNOption(sa) => x.movesPoint(sa) && ((sa <|+| x) != sa)
        case _ => x.movedPoints.isEmpty
      }
    ),

    "support" → forAll((x: A) =>
      (0 to (x.largestMovedPoint.getOrElse(-1) + 10))
        .filter(i => (i <|+| x) != i).toSet == x.movedPoints
    )
  )

  def faithfulPermutationAction(implicit group: Group[A], A: FaithfulPermutationAction[A]) = new PermutationActionProperties(
      name = "faithfulPermutationAction",
      parent = Some(permutationAction),
      bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> ActionLaws[A, Dom].groupAction),
      "support.isEmpty" → forAll((x: A) =>
        x.movedPoints.isEmpty === x.isId
      )
    )

  def permutation(implicit A : PermutationBuilder[A]) = new PermutationActionProperties(
    name = "permutation",
    parent = Some(faithfulPermutationAction),
    bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> ActionLaws[A, Dom].groupAction),

    "images/fromImages" -> forAll((x: A) =>
      A.fromImages(x.images(x.largestMovedPoint.getOrElseFast(-1) + 1)) === x
    )
  )

  class PermutationActionProperties(
    val name: String,
    val parent: Option[PermutationActionProperties],
    val bases: Seq[(String, Laws#RuleSet)],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent
}
