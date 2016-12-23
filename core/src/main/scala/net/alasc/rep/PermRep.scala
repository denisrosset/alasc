package net.alasc.rep

import spire.algebra.Ring
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Rep

trait PermRep[G, K] extends Rep[G, K] {

  implicit def scalar: Ring[K]

  implicit def permutationAction: PermutationAction[G]

  def apply(g: G): scalin.immutable.Mat[K] = {
    import scalin.mutable.dense._
    import scalin.syntax.all._
    val mat = zeros[K](dimension, dimension)
    cforRange(0 until dimension) { i =>
      mat(i, i <|+| g) := scalar.one
    }
    mat.result()
  }

}

trait FaithfulPermRep[G, K] extends PermRep[G, K] {

  implicit def scalar: Ring[K]

  type F <: PermutationAction[G] with Singleton

  implicit def permutationAction: F

}
