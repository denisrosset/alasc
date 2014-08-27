package net.alasc.coll

class Mapped[+T, A, B](val t: T, val f: A => B) {
  def map[C](g: B => C) = new Mapped[T, A, C](t, a => g(f(a)))
}

trait Mappable[T, A] extends Any {
  self: T =>
  def map[B](f: A => B): Mapped[T, A, B] = new Mapped(self, f)
}

trait Container[-A] extends Any {
  def contains(a: A): Boolean
}

object Container 

trait Keyable[-K, A] extends Any {
  def apply(key: K): A
}

trait Searchable[+K, -A] extends Any {
  def find(value: A): Option[K]
}

trait Traversable[+A] extends Any {
  def foreach[U](f: A => U): Unit
}

object Traversable {
  implicit class TraversableMapped[A, B](val m: Mapped[Traversable[A], A, B]) extends AnyVal with Traversable[B] {
    @inline def foreach[U](f: B => U) = { m.t.foreach( a => f(m.f(a)) ) }
  }
}

trait Iterable[+A] extends Any {
  def iterator: Iterator[A]
}

object Iterable {
  implicit class IterableMapped[A, B](val m: Mapped[Iterable[A], A, B]) extends AnyVal with Iterable[B] {
    @inline def iterator = m.t.iterator.map(m.f)
  }
}

trait HasSize extends Any {
  def size: Size
}

sealed trait Size
case class IntSize(size: Int) extends Size
case class BigIntSize(size: BigInt) extends Size
case object InfiniteSize extends Size
