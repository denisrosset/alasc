package net.alasc.coll

import scala.{ specialized => spec }

trait HasSize extends Any {
  def size: Size
}

trait HasForeach[+A] extends Any {
  def foreach[U](f: A => U): Unit
}

trait HasIterator[+A] extends Any {
  def iterator: Iterator[A]
}

trait HasContains[-A] extends Any {
  def contains(a: A): Boolean
}

trait HasFind[@spec(Int) +K, -V] extends Any {
  def find(value: V): Option[K]
}

trait HasIndexing[@spec(Int) K, +V] extends Any {
  def apply(idx: K): V
  def length: K
}
