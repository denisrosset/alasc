package net.alasc.coll

trait BiFunction[B, C] extends Function1[B, C] {
  self =>
  def inverse: BiFunction[C, B] = new InverseBiFunction[C, B](self)
  def inverseApply(c: C): B
  def andThenBi[D](bif: BiFunction[C, D]) = new BiFunction[B, D] {
    def apply(b: B): D = bif(self(b))
    def inverseApply(d: D) = self.inverseApply(bif.inverseApply(d))
  }
  def composeBi[A](bif: BiFunction[A, B]) = new BiFunction[A, C] {
    def apply(a: A): C = self(bif(a))
    def inverseApply(c: C) = bif.inverseApply(self.inverseApply(c))
  }
}

object BiFunction {
  def apply[A, B](f: A => B, fInv: B => A) = new BiFunction[A, B] {
    def apply(a: A): B = f(a)
    def inverseApply(b: B): A = fInv(b)
  }
}

class InverseBiFunction[A, B](of: BiFunction[B, A]) extends BiFunction[A, B] {
  override def inverse: BiFunction[B, A] = of
  def apply(a: A): B = of.inverseApply(a)
  def inverseApply(b: B): A = of.apply(b)
}
