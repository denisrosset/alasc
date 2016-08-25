package net.alasc.laws

import spire.math._

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}

/** Wraps a "small" element of a given type. Used to perform property-based checks that would otherwise be too expensive. */
case class Small[G](val underlying: G) extends AnyVal

object Small {

  implicit def resized[G:Arbitrary]: Arbitrary[Small[G]] = Arbitrary {
    Gen.parameterized( p => Gen.resize(max(p.size / 10, 3), arbitrary[G]).map(Small(_)) )
  }

}
