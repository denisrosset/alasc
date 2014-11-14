package net.alasc.algebra

trait Lattice[A] extends spire.algebra.PartialOrder[A] with spire.algebra.lattice.Lattice[A]
trait BoundedBelowLattice[A] extends Lattice[A] with spire.algebra.lattice.BoundedJoinSemilattice[A]
trait BoundedLattice[A] extends BoundedBelowLattice[A] with spire.algebra.lattice.BoundedLattice[A]

