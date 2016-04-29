package net.alasc.print

/**
  * Created by rossetd0 on 2016/4/29.
  */
trait Format { self =>

  type Inner

  type Output

  type Self <: Format with Singleton

  def finalize(inner: Inner, options: Options): Output

  def inner[A](a: A, options: Options)(implicit print: Print[A, self.type]): Inner = print(a, options)

  def apply[A](a: A, options: Options = Options.empty)(implicit print: Print[A, self.type]): Output = finalize(inner(a, options), options)

}
