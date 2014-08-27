package net.alasc.coll

trait Traversable[+A] extends Any {
  def foreach[U](f: A => U): Unit
}

object Traversable {
  implicit class TraversableMapped[A, B](val m: Mapped[Traversable[A], A, B]) extends AnyVal with Traversable[B] {
    @inline def foreach[U](f: B => U) = { m.t.foreach( a => f(m.f(a)) ) }
  }
}
