package net.alasc.perms

import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.action._

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Rep

trait PermRep[G] extends Rep[G, Rational] {

  implicit def permutationAction: PermutationAction[G]

  def apply(g: G): scalin.immutable.Mat[Rational] = {
    import scalin.mutable.dense._ // TODO: use sparse matrices
    import scalin.syntax.all._
    val mat = zeros[Rational](dimension, dimension)
    cforRange(0 until dimension) { i =>
      mat(i, i <|+| g) := Rational.one
    }
    mat.result()
  }

}

trait FaithfulPermRep[G] extends PermRep[G] {

  type F <: PermutationAction[G] with Singleton

  implicit def permutationAction: F

}
