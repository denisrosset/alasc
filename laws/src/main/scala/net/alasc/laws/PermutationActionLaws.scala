package net.alasc.laws

import spire.algebra._

import org.typelevel.discipline.Laws
import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.arbitrary
import org.scalacheck.Prop._
import spire.syntax.all._
import spire.std.boolean._
import spire.std.int._
import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._
import spire.laws.{ActionLaws, GroupLaws}
import net.alasc.perms.Perm

object PermutationActionLaws {

  def apply[A:Eq:Arbitrary] = new PermutationActionLaws[A] {
    def Equ = implicitly
    def Arb = implicitly
  }

  def genPoint[A:PermutationAction](a: A): Gen[Int] =
    if (a.movesAnyPoint) {
      val mn = spire.math.max(a.smallestMovedPoint.get - 10, 0)
      val lmp = a.largestMovedPoint.get
      val mx = if (lmp < Int.MaxValue + 10) lmp + 10 else lmp
      Gen.choose(mn, mx)
    } else Gen.choose(0, 1000)

}

trait PermutationActionLaws[A] extends Laws {

  import PermutationActionLaws.genPoint

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def actionLaws: ActionLaws[A, Int] = ActionLaws[A, Int](implicitly, implicitly, implicitly, Arbitrary(arbitrary[Dom].map(_.value)))

  def permutationAction(implicit group: Group[A], A: PermutationAction[A]) = new PermutationActionProperties(
    name = "permutationAction",
    parent = None,
    bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> actionLaws.groupAction),

    "isFaithful" -> forAll((x: A) =>
      !A.isFaithful || (x.isId == !x.movesAnyPoint)
    ),

    "findMovedPoint" -> forAll((x: A) =>
      x.findMovedPoint match {
        case NNOption(sa) => x.movesPoint(sa)
        case _ => !x.movesAnyPoint
      }
    ),

    "movedPointsUpperBound" -> forAll((x: A) =>
      x.movedPointsUpperBound match {
        case NNOption(sm) => sm >= x.largestMovedPoint.getOrElseFast(0)
        case _ => !x.movesAnyPoint
      }
    ),

    "movesAnyPoint" -> forAll((x: A) =>
      x.movesAnyPoint == x.movedPoints.nonEmpty
    ),

    "movesPoint" -> forAll((x: A) =>
      forAll(genPoint(x)) { i =>
        ((i <|+| x) != i) == x.movesPoint(i)
      }
    ),

    "nMovedPoints" -> forAll((x: A) =>
      x.movedPoints.size == x.nMovedPoints
    ),

    "movedPoints" -> forAll((x: A) =>
      forAll(genPoint(x)) { i =>
        x.movedPoints.contains(i) == x.movesPoint(i)
      }
    ),

    "largestMovedPoint" -> forAll((x: A) =>
      x.largestMovedPoint match {
        case NNOption(sm) => x.movedPoints.max === sm && ((sm <|+| x) != sm)
        case _ => x.movedPoints.isEmpty
      }
    ),

    "smallestMovedPoint" -> forAll((x: A) =>
      x.smallestMovedPoint match {
        case NNOption(sm) => x.movedPoints.min === sm && ((sm <|+| x) != sm)
        case _ => x.movedPoints.isEmpty
      }
    ),

    "signPerm under conjugation" -> forAll((x: A, y: A) =>
      x.signPerm == (y.inverse |+| x |+| y).signPerm
    ),

    "cycle structure under conjugation" -> forAll((x: A, y: A) =>
      x.cycleStructure == (y.inverse |+| x |+| y).cycleStructure
    ),

    "permutation order under conjugation" -> forAll((x: A, y: A) =>
      x.permutationOrder == (y.inverse |+| x |+| y).permutationOrder
    ),

    "orbit of any point is invariant under action" -> forAll((x: A) =>
      forAll(genPoint(x)) { i =>
        val o = x.orbit(i)
        o.forall(j => o.contains(j <|+| x))
      }
    ),

    "perm from images is the same as toPerm" -> forAll { (x: A) =>
      val n = x.largestMovedPoint.getOrElse(0) + 1
      val p1 = Perm.fromImages(x.images(n))
      val p2 = x.toPerm
      p1 === p2
    },

    "hasSameAction" -> forAll((x: A) =>
      x.hasSameAction(x.toPerm)
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
