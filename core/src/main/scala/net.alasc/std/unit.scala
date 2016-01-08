package net.alasc.std

import scala.reflect.classTag

import spire.algebra.lattice._
import spire.algebra._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._
import net.alasc.util._

// TODO: workaround until Spire 2.10.2 with unit std support
trait UnitEq extends Eq[Unit] {

  def eqv(x:Unit, y:Unit): Boolean = true
  override def neqv(x:Unit, y:Unit): Boolean = false

}

trait UnitGroup extends Group[Unit] {

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

final class UnitPRepBuilder(implicit val action: FaithfulPermutationAction[Unit]) extends PRepBuilder[Unit] {

  type R = UnitPRep.type

  def classTagR = classTag[UnitPRep.type]

  def build(generators: Iterable[Unit]) = UnitPRep

  object UnitPRep extends BuiltRep[Unit] with FaithfulPRep[Unit] {
    type B = UnitPRepBuilder.this.type
    val builder: B = UnitPRepBuilder.this
    def permutationAction = builder.action
    def represents(g: Unit): Boolean = true
    def size = 1
  }

  object lattice extends Lattice[R] with BoundedJoinSemilattice[R] {
    def zero = UnitPRep
    def join(lhs: R, rhs: R): R = UnitPRep
    def meet(lhs: R, rhs: R): R = UnitPRep
  }
  object partialOrder extends PartialOrder[R] {
    def partialCompare(lhs: R, rhs: R): Double = 0.0
  }

}

class UnitAlgebra extends UnitGroup with UnitPermutation

trait UnitInstances {
  implicit final val UnitEq = new UnitEq { }
  implicit final val UnitAlgebra = new UnitAlgebra
  implicit final val UnitPRepBuilder = new UnitPRepBuilder
}
