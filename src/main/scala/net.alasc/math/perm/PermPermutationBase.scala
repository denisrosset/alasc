package net.alasc.math
package perm

import spire.syntax.signed._
import net.alasc.algebra.Permutation

abstract class PermPermutationBase[P <: SpecPerm[P]] extends Permutation[P] {
  def eqv(x: P, y: P): Boolean = x.specEqv(y)
  def support(p: P) = p.support
  def supportMax(p: P) = p.supportMax
  def supportMin(p: P) = p.supportMin
  def signum(p: P) = p.toCycles.signum
  def inverse(p: P) = p.inverse
  def actr(preimage: Int, p: P) = p.image(preimage)
  def actl(p: P, k: Int) = p.invImage(k)
  def minus(p: P, n: Int): P = p.specMinus(n)
  def plus(p: P, n: Int): P = p.specPlus(n)
  def op(x: P, y: P): P = x.specOp(y)
}
