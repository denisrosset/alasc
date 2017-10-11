package net.alasc.maps

import net.alasc.attributes.Attributable
import net.alasc.finite.Grp

abstract class Homomorphism[A, B] extends Attributable {
  def source: Grp[A]
  def range: Grp[B]
  def kernel: Grp[A]
  def apply(a: A): B
  def isInjective: Boolean
  def isSurjective: Boolean
  def isBijective: Boolean = isInjective && isSurjective
  def preimageRepresentative(b: B): A
}

abstract class Monomorphism[A, B] extends Homomorphism[A, B] {
  def kernel = Grp.trivial[A]
  def isInjective = true
  def preimage(b: B): A = preimageRepresentative(b)
}

abstract class Isomorphism[A, B] extends Monomorphism[A, B] {
  def isSurjective = true
}
