package net.alasc.laws

import spire.algebra._

import org.typelevel.discipline.Laws
import org.scalacheck.{Arbitrary, Prop}
import Arbitrary.arbitrary
import org.scalacheck.Prop._
import spire.syntax.all._
import spire.laws._
import spire.std.boolean._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._

object PermutationActionLaws {

  def apply[A:Eq:Arbitrary] = new PermutationActionLaws[A] {
    def Equ = implicitly
    def Arb = implicitly
  }

}

trait PermutationActionLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def actionLaws: ActionLaws[A, Int] = ActionLaws[A, Int](implicitly, implicitly, implicitly, Arbitrary(arbitrary[Dom].map(_.value)))

  def permutationAction(implicit group: Group[A], A: PermutationAction[A]) = new PermutationActionProperties(
    name = "permutationAction",
    parent = None,
    bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> actionLaws.groupAction),

    "smallestMovedPoint" -> forAll((x: A) =>
      x.smallestMovedPoint match {
        case NNOption(sm) => x.movedPoints.min === sm && ((sm <|+| x) != sm)
        case _ => x.movedPoints.isEmpty
      }
    ),

    "largestMovedPoint" -> forAll((x: A) =>
      x.largestMovedPoint match {
        case NNOption(sm) => x.movedPoints.max === sm && ((sm <|+| x) != sm)
        case _ => x.movedPoints.isEmpty
      }
    ),

    "findMovedPoint" -> forAll((x: A) =>
      x.findMovedPoint match {
        case NNOption(sa) => x.movesPoint(sa) && ((sa <|+| x) != sa)
        case _ => x.movedPoints.isEmpty
      }
    ),

    "movedPoints" -> forAll((x: A) =>
      (0 to (x.largestMovedPoint.getOrElse(-1) + 10))
        .filter(i => (i <|+| x) != i).toSet == x.movedPoints
    ),

    "movesPoint" -> forAll((x: A) =>
      (0 to (x.largestMovedPoint.getOrElse(-1) + 10))
        .filter(i => x.movesPoint(i)).toSet == x.movedPoints
    ),

    "nMovedPoints" -> forAll((x: A) =>
      x.movedPoints.size == x.nMovedPoints
    ),

    "signPerm under conjugation" -> forAll((x: A, y: A) =>
      x.signPerm == (y.inverse |+| x |+| y).signPerm
    ),

    "cycle structure under conjugation" -> forAll((x: A, y: A) =>
      x.cycleStructure == (y.inverse |+| x |+| y).cycleStructure
    )

  )

  def faithfulPermutationAction(implicit group: Group[A], A: PermutationAction[A]) = new PermutationActionProperties(
      name = "faithfulPermutationAction",
      parent = Some(permutationAction),
      bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> actionLaws.groupAction),
      "support.isEmpty" -> forAll((x: A) =>
        (x.nMovedPoints == 0) === x.isId
      )
    )

  class PermutationActionProperties(
    val name: String,
    val parent: Option[PermutationActionProperties],
    val bases: Seq[(String, Laws#RuleSet)],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent

}
