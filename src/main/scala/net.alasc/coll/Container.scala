package net.alasc.coll

trait Container[-A] extends Any {
  def contains(a: A): Boolean
}

object Container {
  implicit class ContainerMapped[A, B](val m: BiMapped[Container[A], A, B]) extends AnyVal with Container[B] {
    @inline def contains(b: B) = m.t.contains(m.f.inverseApply(b))
  }
}
