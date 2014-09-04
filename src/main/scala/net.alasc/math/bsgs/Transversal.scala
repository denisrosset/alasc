package net.alasc.math
package bsgs

import scala.util.Random
import scala.collection.immutable.BitSet

import spire.syntax.group._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._

trait Transversal[P] extends Any {
  def orbitSize: Int
  def inOrbit(b: Int): Boolean
  def orbit: Iterable[Int]
  def foreachOrbit[U](f: Int => U): Unit
  def orbitSet: Set[Int]
  def randomOrbit(rand: Random): Int
  def iterable: Iterable[(Int, InversePair[P])]
  def foreachU[N](f: P => N): Unit
  def uPair(b: Int): InversePair[P]
  def u(b: Int): P
  def uInv(b: Int): P
  def randomU(rand: Random): P
}

object Transversal {
  def empty[P](beta: Int)(implicit algebra: FiniteGroup[P]): Transversal[P] = new EmptyTransversal(beta)
}

class EmptyTransversal[P](val beta: Int)(implicit algebra: FiniteGroup[P]) extends Transversal[P] {
  def orbitSize = 1
  def inOrbit(b: Int) = beta == b
  def orbit = Iterable(beta)
  def foreachOrbit[U](f: Int => U) = { f(beta) }
  def orbitSet = collection.immutable.BitSet(beta)
  def randomOrbit(rand: Random) = beta
  def iterable = Iterable((beta -> algebra.id))
  def foreachU[N](f: P => N) = { f(algebra.id) }
  def uPair(b: Int) = InversePair(algebra.id, algebra.id)
  def u(b: Int) = { require(b == beta); algebra.id }
  def uInv(b: Int) = u(b)
  def randomU(rand: Random) = algebra.id
}
