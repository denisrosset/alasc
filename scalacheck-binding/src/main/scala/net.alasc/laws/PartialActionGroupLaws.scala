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

object PartialActionGroupLaws {
  def apply[G: Eq: Arbitrary, A: Eq: Arbitrary] = new PartialActionGroupLaws[G, A] {
    val scalarLaws = GroupLaws[G]
    def EquA = Eq[A]
    def ArbA = implicitly[Arbitrary[A]]
  }
}

trait PartialActionGroupLaws[G, A] extends Laws {

  val scalarLaws: GroupLaws[G]

  import scalarLaws.{ Equ => EqA, Arb => ArA }

  implicit def EquA: Eq[A]
  implicit def ArbA: Arbitrary[A]

  def semigroupPartialAction(implicit G: PartialAction[A, G], G0: Semigroup[G]) = new PartialActionProperties(
    name = "semigroupPartialAction",
    sl = _.semigroup(G0),
    parent = None,

    "left compatibility" → forAll { (g: G, h: G, a: A) =>
      ( (h ?+|> a) && ((g |+| h) ?+|> a) ) ==>
        (((g |+| h) !|+|> a) === (g !|+|> (h !|+|> a)))
    },

    "right compatibility" → forAll { (a: A, g: G, h: G) =>
      ( (a <|+? g) && (a <|+? (g |+| h)) ) ==>
      ((a <|+|! (g |+| h)) === ((a <|+|! g) <|+|! h))
    }
  )

  def monoidPartialAction(implicit G: PartialAction[A, G], G0: Monoid[G]) = new PartialActionProperties(
    name = "monoidPartialAction",
    sl = _.monoid(G0),
    parent = Some(semigroupPartialAction)
  )

  def groupPartialAction(implicit G: PartialAction[A, G], G0: Group[G]) = new PartialActionProperties(
    name = "groupPartialAction",
    sl = _.group(G0),
    parent = Some(monoidPartialAction)
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
