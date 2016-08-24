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
import net.alasc.domains.{Dom, Domain}
import net.alasc.syntax.all._
import net.alasc.util._

object PermutationActionLaws {

  def apply[A:Eq:Arbitrary](domain0: Domain)(implicit da: Arbitrary[Dom[domain0.type]])  = new PermutationActionLaws[A] {
    val domain: domain0.type = domain0
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def DomArb = da
  }

}

trait PermutationActionLaws[A] extends Laws {

  val domain: Domain

  type D = Dom[domain.type]

  implicit def convertAction(implicit pa: Action[Int, A]): Action[D, A] =
    new Action[D, A] {
      def actr(k: D, a: A): D = Dom(domain)(pa.actr(k, a))
      def actl(a: A, k: D): D = Dom(domain)(pa.actl(a, k))
    }

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  implicit def DomArb: Arbitrary[Dom[domain.type]]

  def permutationAction(implicit group: Group[A], A: PermutationAction[A]) = new PermutationActionProperties(
    name = "permutationAction",
    parent = None,
    bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> ActionLaws[A, D].groupAction),

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
      bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> ActionLaws[A, D].groupAction),
      "support.isEmpty" -> forAll((x: A) =>
        (x.nMovedPoints == 0) === x.isId
      )
    )

  def permutationBuilder(implicit A: PermutationBuilder[A]) = new PermutationActionProperties(
    name = "permutation",
    parent = Some(faithfulPermutationAction),
    bases = Seq("group" -> GroupLaws[A].group, "groupAction" -> ActionLaws[A, D].groupAction),

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
