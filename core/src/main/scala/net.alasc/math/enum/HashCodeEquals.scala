package net.alasc
package math
package enum

import spire.math.Rational

/** Marker typeclass certifying that the type `A` has well-behaved .hashCode and .equals,
  * so that it can be used as a key in Scala collections. */
trait HashCodeEquals[A]

/** Known objects that have a sane `hashCode` and `equals` implementation. */
object HashCodeEquals {
  implicit val byte: HashCodeEquals[Byte] = null
  implicit val short: HashCodeEquals[Short] = null
  implicit val int: HashCodeEquals[Int] = null
  implicit val long: HashCodeEquals[Long] = null
  implicit val double: HashCodeEquals[Double] = null
  implicit val float: HashCodeEquals[Float] = null
  implicit val string: HashCodeEquals[String] = null
  implicit val rational: HashCodeEquals[Rational] = null
  implicit def tuple2[A: HashCodeEquals, B: HashCodeEquals]: HashCodeEquals[(A, B)] = null
  implicit def tuple3[A: HashCodeEquals, B: HashCodeEquals, C: HashCodeEquals]: HashCodeEquals[(A, B, C)] = null
  implicit def tuple4[A: HashCodeEquals, B: HashCodeEquals, C: HashCodeEquals, D: HashCodeEquals]: HashCodeEquals[(A, B, C, D)] = null
  implicit def set[A: HashCodeEquals]: HashCodeEquals[Set[A]] = null
  implicit def seq[A: HashCodeEquals]: HashCodeEquals[Seq[A]] = null
  implicit def map[A: HashCodeEquals, B: HashCodeEquals]: HashCodeEquals[Map[A, B]] = null
}
