package net.alasc

import spire.algebra.Ring

import cyclo.Cyclo
import scalin.algebra.{MatRing, Pivot}
import scalin.syntax.all._

import spire.syntax.action._
import spire.syntax.eq._
import spire.std.int._

import net.alasc.algebra.PermutationAction

package object gap3 {

  implicit object CycloPivot extends Pivot[Cyclo] {
    override def priority(a: Cyclo): Double = a.nTerms
    override def closeToZero(a: Cyclo): Boolean = a.isZero
  }

  def directSum[A, MA <: scalin.Mat[A]](lhs: scalin.Mat[A], rhs: scalin.Mat[A])(implicit A: MatRing[A, MA]): MA = {
    import scalin.syntax.all._
    (lhs horzcat zeros[A](lhs.nRows, rhs.nCols)) vertcat
      (zeros[A](rhs.nRows, lhs.nCols) horzcat rhs)
  }

  def iverson[A:Ring](predicate: Boolean) =
    if (predicate) Ring[A].one else Ring[A].zero

  class PermMatOps[A](val param: Null) extends AnyVal {

    def apply[G:PermutationAction, MA <: scalin.Mat[A]](g: G, n: Int)(implicit A: MatRing[A, MA]): MA = {
      import A.scalar
      tabulate[A](n, n)( (i, j) => iverson[A]((i <|+| g) === j) )
    }

  }

  def permMat[A]: PermMatOps[A] = new PermMatOps[A](null)

}
