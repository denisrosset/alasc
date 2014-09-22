package net.alasc.algebra

import spire.algebra.PartialOrder

trait Lattice[A] extends PartialOrder[A] {
  def join(x: A, y: A): A
  def meet(x: A, y: A): A
}

trait BoundedBelowLattice[A] extends Lattice[A] {
  def zero: A
  def isZero(a: A) = eqv(zero, a)
}

trait BoundedLattice[A] extends BoundedBelowLattice[A] {
  def one: A
  def isOne(a: A) = eqv(one, a)
}
