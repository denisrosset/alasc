package net.alasc.tests

import spire.algebra.Eq

import org.scalactic.TripleEqualsSupport.{AToBEquivalenceConstraint, BToAEquivalenceConstraint}
import org.scalactic._

// The code in this file was taken from Cats, which is turn contains
// the following comment:

// The code in this file was taken and only slightly modified from
// https://github.com/bvenners/equality-integration-demo
// Thanks for the great examples, Bill!

final class AlascEquivalence[T](T: Eq[T]) extends Equivalence[T] {

  def areEquivalent(a: T, b: T): Boolean = T.eqv(a, b)

}

trait LowPriorityStrictAlascConstraints extends TripleEquals {

  implicit def lowPriorityCatsCanEqual[A, B](implicit B: Eq[B], ev: A <:< B): CanEqual[A, B] =
    new AToBEquivalenceConstraint[A, B](new AlascEquivalence(B), ev)

}

trait StrictAlascEquality extends LowPriorityStrictAlascConstraints {

  override def convertToEqualizer[T](left: T): Equalizer[T] = super.convertToEqualizer[T](left)

  implicit override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): CanEqual[A, B] = super.unconstrainedEquality[A, B]

  implicit def alascCanEqual[A, B](implicit A: Eq[A], ev: B <:< A): CanEqual[A, B] =
    new BToAEquivalenceConstraint[A, B](new AlascEquivalence(A), ev)

}

object StrictAlascEquality extends StrictAlascEquality
