package net.alasc.math
package perm

object LongBits {
  @inline final def rightFill(n: Int): Long = ((1L << n) - 1) - ((n & 64) >> 6)
  @inline final def leftFill(n: Int): Long = ~rightFill(64 - n)
}
