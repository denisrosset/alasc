package net.alasc.print

import spire.NoImplicit
import spire.algebra.{Eq, MultiplicativeMonoid, Ring}
import spire.math.{Rational, SafeLong}
import spire.util.Opt

import cyclo.Cyclo
import scalin.{Mat, Vec}
import scalin.algebra.VecEngine


/** Prints objects of type `A` into the internal representation of the specified format.
  *
  * @tparam A Type of objects to be printed.
  * @tparam F Singleton type of the output format.
  */
trait Print[-A, F <: Format with Singleton] {

  /** Returns a description of the object `a` in the internal representation of format `F`. */
  def apply(a: A, options: Options): F#Internal

}

object Print {

  /** Constructs a `Print` instance that prints objects of type `A` into the specified `format`.
    *
    * @param format        Destination format.
    * @param printFunction Provided printing function.
    * @tparam A            Type of printed elements.
    * @return              A `Print` instance that knows how to write objects of type `A` in the internal representation
    *                      of the given format.
    */
  def apply[A](format: Format)(printFunction: (A, Formatter[format.type]) => format.Internal): Print[A, format.type] = new Print[A, format.type] {

    def apply(a: A, options: Options) = printFunction(a, Formatter(format, options))

  }

}
