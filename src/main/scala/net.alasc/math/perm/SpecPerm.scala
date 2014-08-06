package net.alasc.math
package perm

abstract class SpecPerm[P <: SpecPerm[P]] extends Perm {
  def inverse: P
  def specEqv(rhs: P): Boolean
  def specOp(rhs: P): P
  def specPlus(n: Int): P
  def specMinus(n: Int): P
}
