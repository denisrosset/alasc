package net.alasc
package coll

trait Traversable[+A] extends Any with HasForeach[A] with HasSize {
  override def toString = s"Traversable of size $size"
}

trait Iterable[+A] extends Any with Traversable[A] with HasIterator[A] {
  override def toString = s"Iterable of size $size"
}

trait Set[A] extends Any with Iterable[A] with HasContains[A] {
  override def toString = s"Set of size $size"
}
