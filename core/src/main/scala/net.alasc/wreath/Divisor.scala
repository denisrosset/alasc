package net.alasc.wreath

case class Divisor(d: Int, max: Int, offset: Int, f: Long, shift: Int) {

  def divide(x: Int) =
    if (x > max)
      x / d
    else
      (((x.toLong + offset) * f) >>> shift).toInt

}

object Divisor {

  def numberOfSignificantBits(i: Int) = 32 - java.lang.Integer.numberOfLeadingZeros(i)

  def numberOfSignificantBits(i: Long) = 64 - java.lang.Long.numberOfLeadingZeros(i)

  def apply(maxFast: Int, d: Int): Divisor = {
    val w = numberOfSignificantBits(maxFast)
    val b = numberOfSignificantBits(d) - 1
    val r = w + b
    val f: Double = (1L << r).toDouble / d.toDouble
    val rn: Long = scala.math.round(f)
    val rem: Double = f - rn.toDouble
    if (rem == 0)
      Divisor(d, Int.MaxValue, 0, 1, b)
    else if (rem > 0) { // fractional part < 0.5
      val maxWidth = 64 - numberOfSignificantBits(rn) - w
      val max = ((1L << maxWidth) - 1).min((1L << w) - 1).toInt
      Divisor(d, max, 1, rn, r)
    } else {
      val maxWidth = 64 - numberOfSignificantBits(rn) - w
      val max = (1L << maxWidth).min((1L << w) - 1).toInt
      Divisor(d, max, 0, rn, r)
    }
  }

}
