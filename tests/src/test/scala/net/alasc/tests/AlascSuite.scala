package net.alasc.tests

import spire.algebra.Eq
import spire.syntax.EqOps

import org.scalacheck.Shrink
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

import net.alasc.laws.NestedDiscipline

/**
  * An opinionated stack of traits to improve consistency and reduce
  * boilerplate in Alasc tests (inspired by Cats).
  */
trait AlascSuite extends FunSuite with Matchers
  with PropertyChecks
  with Discipline with NestedDiscipline
  with StrictAlascEquality
  with spire.syntax.AllSyntax with spire.std.AnyInstances
  with net.alasc.syntax.AllSyntax with net.alasc.std.AnyInstances
  with spire.syntax.GroupoidSyntax {

  // disable Eq syntax (by making `eqOps` not implicit), since it collides
  // with scalactic's equality
  override def eqOps[A:Eq](a:A): EqOps[A] = new EqOps[A](a)

  def discardEvaluation(): Nothing = throw new DiscardedEvaluationException

  def noShrink[T] = Shrink[T](_ => Stream.empty)

}
