package net.alasc

import spire.algebra.{Eq, Ring}

import cyclo.Cyclo
import scalin.Pivot
import scalin.immutable.{Mat, MatEngine}
import scalin.syntax.all._

import spire.syntax.action._
import spire.syntax.eq._
import spire.std.int._
import spire.util.Opt

import net.alasc.algebra.PermutationAction

package object gap3 {

  implicit object CycloPivot extends Pivot[Cyclo] {
    override def priority(a: Cyclo): Double = a.nTerms
    override def closeToZero(a: Cyclo): Boolean = a.isZero
    def optionalExactEq = Opt(Eq[Cyclo])
  }

  def directSum[A:MatEngine:Ring](lhs: scalin.Mat[A], rhs: scalin.Mat[A]): Mat[A] = {
    (lhs horzcat Mat.zeros[A](lhs.nRows, rhs.nCols)) vertcat
    (Mat.zeros[A](rhs.nRows, lhs.nCols) horzcat rhs)
  }

  def iverson[A:Ring](predicate: Boolean) =
    if (predicate) Ring[A].one else Ring[A].zero

  class PermMatOps[A](val param: Null) extends AnyVal {

    def apply[G:PermutationAction](g: G, n: Int)(implicit A: Ring[A], MA: MatEngine[A]): Mat[A] = {
      Mat.tabulate[A](n, n)( (i, j) => iverson[A]((i <|+| g) === j) )
    }

  }

  def permMat[A]: PermMatOps[A] = new PermMatOps[A](null)

}
