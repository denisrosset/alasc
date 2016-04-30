package net.alasc.print

case class Formatter[F <: Format with Singleton](format: F, options: Options) {

  def apply[A](a: A)(implicit print: Print[A, F]): F#Internal = print(a, options)

}

/** Defines a pretty-printing format. This trait is designed to be implemented by singleton objects. */
trait Format { self =>

  /** Intermediate internal representation of the output in construction. */
  type Internal

  /** Final output. */
  type Output

  /** Transforms the internal representation to the output. */
  def finalize(internal: Internal, options: Options): Output

  def apply[A](a: A, options: Options = Options.empty)(implicit print: Print[A, self.type]): Output = finalize(print(a, options), options)

}
