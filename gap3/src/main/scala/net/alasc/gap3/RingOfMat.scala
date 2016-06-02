package net.alasc.gap3

import spire.algebra.Ring

import scalin.algebra.MatRing
import scalin.immutable.Mat

import scalin.syntax.all._

class RingOfMat[K, MK <: Mat[K]](val d: Int)(implicit MK: MatRing[K, MK]) extends Ring[MK] {

  override def one: MK = MK.eye(d)

  override def negate(x: MK): MK = MK.negate(x)

  override def zero: MK = MK.zeros(d, d)

  override def times(x: MK, y: MK): MK = MK.times(x, y)

  override def plus(x: MK, y: MK): MK = MK.plus(x, y)

  override def fromInt(n: Int): MK = MK.times(MK.eye(d), MK.scalar.fromInt(n))

  override def minus(x: MK, y: MK): MK = MK.minus(x, y)

}
