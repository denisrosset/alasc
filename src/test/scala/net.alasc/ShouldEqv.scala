package org.scalatest

import org.scalatest.MatchersHelper.newTestFailedException
import spire.algebra.Eq

trait EqMatchers {
  final implicit class ShouldEqvWrapper[T](val lhs: T) {
    def shouldEqv(rhs: T)(implicit ev: Eq[T]) {
      if (!ev.eqv(lhs, rhs)) {
        val (leftee, rightee) = org.scalatest.Suite.getObjectsForFailureMessage(lhs, rhs)
        throw newTestFailedException(FailureMessages("wasNotEqvTo", leftee, rightee))
      }
    }
  }
}
