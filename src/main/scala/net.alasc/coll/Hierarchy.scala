package net.alasc
package coll

trait Traversable[+A] extends Any with HasForeach[A] with HasSize

trait Iterable[+A] extends Any with Traversable[A] with HasIterator[A]

trait Set[A] extends Any with Iterable[A] with HasContains[A]
