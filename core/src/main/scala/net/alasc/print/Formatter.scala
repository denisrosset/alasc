package net.alasc.print

/**
  * Created by rossetd0 on 2016/4/29.
  */
case class Formatter[F <: Format with Singleton](format: F, options: Options) {

  def apply[A](a: A)(implicit print: Print[A, F]): F#Inner = print(a, options)

}
