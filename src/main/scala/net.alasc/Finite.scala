package net.alasc
import scala.util.Random

trait GenFinite extends Any {
  /** Tests if this group element is the identity. */
  def isIdentity: Boolean
  /** Returns the inverse of this group element. */
  def inverse: GenFinite
  def hash: Int
  /** Computes the order of this group element.
    * 
    * The order of this group element is the number k such that
    * this.power(k) == this.
    */
  def order: Int
  /** Returns the k-th power of this element.
    * 
    * Computes this*this*this... k times.
    * 
    * @param k  Power to compute (>= 0)
    * @return   The computed power of this element.
    */
  def power(k: Int): GenFinite
}

trait Finite[F <: Finite[F]] extends Any with GenFinite {
  self: F =>
  def dontForgetToOverrideHashCodeAndEquals: Boolean
  def inverse: F
  def *(that: F): F
  def ===(that: F): Boolean
  def power(k: Int): F
  def conjugatedBy(f: F): F
}

trait GenFiniteLike extends GenFinite

trait FiniteLike[F <: Finite[F]] extends Any with Finite[F] {
  self: F =>
  def power(k: Int) = k match {
    case 0 => this*inverse
    case _ => powerRec(k - 1, this)
  }
  @annotation.tailrec private def powerRec(k: Int, acc: F): F = k match {
    case 0 => acc
    case _ => powerRec(k - 1, this*acc)
  }
  def order = orderRec(1, this)
  @annotation.tailrec private def orderRec(k: Int, acc: F): Int = acc.isIdentity match {
    case true => k
    case false => orderRec(k + 1, this*acc)
  }
  def conjugatedBy(f: F) = (f.inverse) * this * f
}
