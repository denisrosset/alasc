package net.alasc.algebra

trait Check[T] {
  def check(t: T): Unit
}
