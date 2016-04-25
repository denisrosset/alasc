package net.alasc.gap3

import cyclo.Cyclo
import scalin.immutable.{DenseMat, Mat, Vec}
import scalin.syntax.all._
import spire.syntax.action._
import spire.syntax.eq._
import spire.std.int._
import spire.syntax.field._
import net.alasc.syntax.all._
import scalin.immutable.dense._

import net.alasc.perms.Perm

trait AMat {

  def dimension: Int

  def value: Mat[Cyclo]

}

case class ProductAMat(a: AMat, b: AMat) extends AMat {
  require(a.dimension == b.dimension)
  def dimension = a.dimension
  def value = a.value * b.value
}

case class IdentityPermAMat(dimension: Int) extends AMat {
  def value = eye[Cyclo](dimension)
}

case class AMatPerm(p: Perm, dimension: Int) extends AMat {
  def value = DenseMat.tabulate[Cyclo](dimension, dimension) { (i, j) =>
    if ((i <|+| p) === j)
      Cyclo.one
    else
      Cyclo.zero
  }
}

case class AMatMon(mon: Mon) extends AMat {
  def dimension = mon.diag.length

  def value = AMatPerm(mon.perm, mon.diag.length).value * DiagonalAMat(mon.diag).value
}

case class AllOneAMat(dimension: Int) extends AMat {
  def value = ones[Cyclo](dimension, dimension)
}

case class NullAMat(dimension: Int) extends AMat {
  def value = zeros[Cyclo](dimension, dimension)
}

case class DiagonalAMat(vec: Vec[Cyclo]) extends AMat {
  def dimension = vec.length

  def value = DenseMat.tabulate(vec.length, vec.length) { (i, j) => if (i === j) vec(i) else Cyclo.zero }
}

case class DFTAMat(dimension: Int) extends AMat {
  def value = DenseMat.tabulate(dimension, dimension) { (i, j) => Cyclo.e(dimension).pow(i * j) }
}

case class SORAMat(dimension: Int) extends AMat {
  def value = DenseMat.tabulate(dimension, dimension) { (i, j) =>
    if (i === 0 || j === 0) Cyclo.one
    else if (i === j) -Cyclo.one // TODO: have Cyclo.minusOne
    else Cyclo.zero
  }
}

case class ScalarMultipleAMat(s: Cyclo, a: AMat) extends AMat {
  def dimension = a.dimension

  def value = s *: a.value
}

case class PowerAMat(a: AMat, n: Int) extends AMat {
  // TODO: optimize with mat.pow() when included in scalin
  def dimension = a.dimension

  def value = pow(a.value, n)

  protected def pow(m: Mat[Cyclo], k: Int): Mat[Cyclo] =
    if (k < 0) pow(m, -k).inverse
    else if (k === 0) eye[Cyclo](dimension)
    else if (k === 1) m
    else m * pow(m, k - 1)
}

case class ConjugateAMat(a: AMat, b: AMat) extends AMat {
  require(a.dimension == b.dimension)
  val value = b.value.inverse * a.value * b.value

  def dimension = a.dimension
}

case class DirectSumAMat(a: AMat*) extends AMat {
  def dimension = a.foldLeft(0) { (d, aa) => d + aa.dimension }
  import scalin.immutable.dense._
  def value = a.map(_.value).foldLeft(zeros[Cyclo](0, 0): Mat[Cyclo]) { case (lhs, rhs) => directSum(lhs, rhs) }
}

case class TensorProductAMat(a: AMat*) extends AMat {
  def dimension = a.foldLeft(1) { (d, aa) => d * aa.dimension }

  def value = a.foldLeft(eye[Cyclo](1): Mat[Cyclo]) { case (lhs, rhs) => lhs kron rhs.value }

}

case class GaloisConjugateAMat(a: AMat, k: Int) extends AMat {
  def dimension = a.dimension

  def value = a.value.map(c => c.galois(k))
}
