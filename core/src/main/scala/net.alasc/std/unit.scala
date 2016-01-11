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

class UnitAlgebra extends UnitGroup with UnitPermutation

trait UnitInstances {
  implicit final val UnitEq = new UnitEq { }
  implicit final val UnitAlgebra = new UnitAlgebra
  implicit final val UnitPRepBuilder = new UniquePRepBuilder(1)(UnitAlgebra)
}
