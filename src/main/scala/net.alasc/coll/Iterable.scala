package net.alasc.coll

trait Iterable[+A] extends Any {
  def iterator: Iterator[A]
}

object Iterable {
  implicit class IterableMapped[A, B](val m: Mapped[Iterable[A], A, B]) extends AnyVal with Iterable[B] {
    @inline def iterator = m.t.iterator.map(m.f)
  }
}
