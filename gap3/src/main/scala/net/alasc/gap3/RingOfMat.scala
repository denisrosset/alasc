package net.alasc.gap3

import spire.algebra.Ring

import scalin.MatEngine
import scalin.immutable.Mat

import scalin.syntax.all._

class RingOfMat[K, MK <: Mat[K]](val d: Int)(implicit MK: MatEngine[K, MK], K: Ring[K]) extends Ring[MK] {

  override def one: MK = MK.eye(d)

  override def negate(x: MK): MK = MK.negate(x)

  override def zero: MK = MK.zeros(d, d)

  override def times(x: MK, y: MK): MK = MK.times(x, y)

  override def plus(x: MK, y: MK): MK = MK.plus(x, y)

  override def fromInt(n: Int): MK = MK.times(MK.eye(d), K.fromInt(n))

  override def minus(x: MK, y: MK): MK = MK.minus(x, y)

}
