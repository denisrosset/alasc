package net.alasc.coll

class Mapped[+T, A, B](val t: T, val f: Function1[A, B]) {
  def map[C](g: Function1[B, C]) = new Mapped[T, A, C](t, a => g(f(a)))
}

class BiMapped[+T, A, B](t: T, override val f: BiFunction[A, B]) extends Mapped[T, A, B](t, f) {
  def map[C](g: BiFunction[B, C]) = new BiMapped[T, A, C](t, f.andThenBi(g))
}

trait Mappable[T, A] extends Any {
  self: T =>
  def map[B](f: A => B): Mapped[T, A, B] = new Mapped(self, f)
  def bimap[B](f: A => B, fInv: B => A): BiMapped[T, A, B] = new BiMapped(self, BiFunction(f, fInv))
  def bimap[B](bifun: BiFunction[A, B]): BiMapped[T, A, B] = new BiMapped(self, bifun)
}
