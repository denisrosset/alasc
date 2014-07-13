package net.alasc

import scala.{ specialized => spec }
import scala.language.implicitConversions
import spire.algebra.Semigroup

trait FiniteInstances {
  implicit def FiniteSemigroup[F <: Finite[F]] = new Semigroup[F] {
    def op(x: F, y: F): F = x * y
  }
}

trait IndexInstances {
  implicit def ArrayIndex[@spec(Int) A]: Index[Array[A], A] = new ArrayIndex[A]
  implicit def IndexedSeqIndex[@spec(Int) A]: Index[IndexedSeq[A], A] = new IndexedSeqIndex[A]
}

trait PermutingInstances {
  implicit def IndexedSeqPermutingAction[@spec(Int) A, P <: Permuting[P]]: IndexedSeqPermutingAction[A, P] =
    new IndexedSeqPermutingAction[A, P]
}

trait AllInstances extends
    FiniteInstances with
    IndexInstances with
    PermutingInstances
