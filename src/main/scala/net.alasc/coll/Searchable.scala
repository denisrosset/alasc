package net.alasc.coll

trait Searchable[+K, -A] extends Any {
  def find(value: A): Option[K]
}

object Searchable {
  implicit class SearchableMapped[K, A, B](val m: BiMapped[Searchable[K, A], A, B]) extends AnyVal with Searchable[K, B] {
    @inline def find(value: B) = m.t.find(m.f.inverseApply(value))
  }
}
