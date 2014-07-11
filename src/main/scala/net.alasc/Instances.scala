package net.alasc

import scala.{ specialized => spec }
import scala.language.implicitConversions

trait IndexInstances {
  implicit def ArrayIndex[@spec(Int) A]: Index[A, Array[A]] = new ArrayIndex[A]
  implicit def IndexedSeqIndex[@spec(Int) A]: Index[A, IndexedSeq[A]] = new IndexedSeqIndex[A]
}

trait PermutingInstances {
  implicit def IndexedSeqPermutingAction[@spec(Int) A, P <: Permuting[P]]: IndexedSeqPermutingAction[A, P] =
    new IndexedSeqPermutingAction[A, P]
}

trait AllInstances extends
    IndexInstances with
    PermutingInstances
