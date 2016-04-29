package net.alasc.print

import spire.NoImplicit
import spire.algebra.{Eq, MultiplicativeMonoid, Ring}
import spire.math.{Rational, SafeLong}
import spire.util.Opt

import cyclo.Cyclo
import scalin.{Mat, Vec}
import scalin.algebra.VecEngine

trait Print[-A, F <: Format with Singleton] {

  def apply(a: A, options: Options): F#Inner

}

object Print {


  def apply[A](format: Format)(printFunction: (A, Formatter[format.type]) => format.Inner): Print[A, format.type] = new Print[A, format.type] {

    def apply(a: A, options: Options) = printFunction(a, Formatter[format.type](format, options))

  }

}
