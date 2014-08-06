package net.alasc.algebra

trait Length[T] {
  def length(t: T): Int
}

trait BigLength[T] {
  def bigLength(t: T): BigInt
}
