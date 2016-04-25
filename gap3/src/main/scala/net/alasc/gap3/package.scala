package net.alasc

import cyclo.Cyclo
import scalin.Mat
import scalin.algebra.{MatRing, Pivot}

package object gap3 {

  implicit object CycloPivot extends Pivot[Cyclo] {
    override def priority(a: Cyclo): Double = a.nTerms
    override def closeToZero(a: Cyclo): Boolean = a.isZero
  }

  def directSum[A, MA <: Mat[A]](lhs: Mat[A], rhs: Mat[A])(implicit A: MatRing[A, MA]): MA = {
    import scalin.syntax.all._
    (lhs horzcat zeros[A](lhs.nRows, rhs.nCols)) vertcat
      (zeros[A](rhs.nRows, lhs.nCols) horzcat rhs)
  }

}
