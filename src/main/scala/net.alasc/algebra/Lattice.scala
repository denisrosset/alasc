package net.alasc.algebra

import spire.algebra.PartialOrder

trait JoinSemilattice[A] extends PartialOrder[A] {
  def join(x: A, y: A): A
}

trait MeetSemilattice[A] extends PartialOrder[A] {
  def meet(x: A, y: A): A
}

trait Lattice[A] extends JoinSemilattice[A] with MeetSemilattice[A]
