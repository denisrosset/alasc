package net.alasc.print

/**
  * Created by rossetd0 on 2016/4/29.
  */
object Ascii extends Format {

  type Inner = String

  type Output = String

  def finalize(inner: String, options: Options) = inner

}
