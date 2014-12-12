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
import spire.std.option._

import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._

object PartialActionLaws {
  def apply[G: Eq: Arbitrary, A: Eq: Arbitrary] = new PartialActionLaws[G, A] {
    val scalarLaws = GroupoidLaws[G]
    def EquA = Eq[A]
    def ArbA = implicitly[Arbitrary[A]]
  }
}

trait PartialActionLaws[G, A] extends Laws {

  val scalarLaws: GroupoidLaws[G]

  import scalarLaws.{ Equ => EqA, Arb => ArA }

  implicit def EquA: Eq[A]
  implicit def ArbA: Arbitrary[A]

  def semigroupoidPartialAction(implicit G: PartialAction[A, G], G0: Semigroupoid[G]) = new PartialActionProperties(
    name = "semigroupoidPartialAction",
    sl = _.semigroupoid(G0),
    parent = None,

    "left compatibility" → forAll { (g: G, h: G, a: A) =>
      ( (h ?+|> a) && (g ?+? h) ) ==>
      ((g |+|! h) ?+|> a) && ((g |+|! h) !|+|> a) === (g !|+|> (h !|+|> a))
    },

    "right compatibility" → forAll { (a: A, g: G, h: G) =>
      ( (a <|+? g) && (g ?+? h) ) ==>
      (a <|+? (g |+|! h)) && ((a <|+|! (g |+|! h)) === ((a <|+|! g) <|+|! h))
    }
  )

  def partialMonoidPartialAction(implicit G: PartialAction[A, G], G0: PartialMonoid[G]) = new PartialActionProperties(
    name = "partialMonoidPartialAction",
    sl = _.partialMonoid(G0),
    parent = Some(semigroupoidPartialAction),

    "right identity" → forAll { (g: G, a: A) =>
      (g ?+|> a) ==>
      ((g.rightId ?+|> a) && ((g.rightId !|+|> a) === a))
    },

    "left identity" → forAll { (g: G, a: A) =>
      (a <|+? g) ==>
      ((a <|+? g.leftId) && ((a <|+|! g.leftId) === a))
    }
  )

  def groupoidPartialAction(implicit G: PartialAction[A, G], G0: Groupoid[G]) = new PartialActionProperties(
    name = "groupoidPartialAction",
    sl = _.groupoid(G0),
    parent = Some(partialMonoidPartialAction),

    "left and right partial action compatibility" → forAll { (a: A, g: G) =>
      (a <|+? g) ==>
      ((g.inverse ?+|> a) && ((a <|+|! g) === (g.inverse !|+|> a)))
    }
  )

  class PartialActionProperties(
    val name: String,
    val sl: scalarLaws.type => scalarLaws.RuleSet,
    val parent: Option[PartialActionProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Seq("scalar" → sl(scalarLaws))
    val parents = parent.toSeq
  }
}
