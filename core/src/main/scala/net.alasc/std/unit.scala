package net.alasc
package std

import scala.reflect.classTag

import spire.algebra.lattice._
import spire.algebra._
import spire.util.Opt

import algebra._
import util._

trait UnitFiniteGroup extends FiniteGroup[Unit] {
  def inverse(g: Unit): Unit = ()
  def eqv(x: Unit, y: Unit): Boolean = true
  def op(x: Unit, y: Unit): Unit = ()
  def id: Unit = ()
}

trait UnitPermutation extends FaithfulPermutationAction[Unit] {
  def actl(g: Unit, p: Int): Int = p
  override def actr(p: Int, g: Unit): Int = p
  def support(g: Unit): Set[Int] = Set.empty[Int]
  def supportMax(g: Unit) = NNNone
  def supportMin(g: Unit) = NNNone
  def supportMaxElement: Int = -1
}

final class UnitRepresentations(implicit action: FaithfulPermutationAction[Unit])  extends Representations[Unit] { self =>
  def get(generators: Iterable[Unit]) = Representation
  type R = Representation[Unit]
  val RClassTag = classTag[R]
  object Representation extends R {
    def action = self.action
    def represents(g: Unit): Boolean = true
    val representations = Opt(self)
    def size = 1
  }
  object lattice extends Lattice[R] with BoundedJoinSemilattice[R] {
    def zero = Representation
    def join(lhs: R, rhs: R): R = Representation
    def meet(lhs: R, rhs: R): R = Representation
  }
  object partialOrder extends PartialOrder[R] {
    def partialCompare(lhs: R, rhs: R): Double = 0.0
  }
}

class UnitAlgebra extends UnitFiniteGroup with UnitPermutation

trait UnitInstances {
  implicit final val UnitAlgebra = new UnitAlgebra
  implicit final val UnitRepresentations = new UnitRepresentations
}
