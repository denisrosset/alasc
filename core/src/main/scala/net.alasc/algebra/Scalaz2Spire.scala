package net.alasc.algebra

trait Scalaz2SpireSemigroup[F] extends Any with spire.algebra.Semigroup[F] {
  def instance: scalaz.Semigroup[F]
  def op(x: F, y: F): F = instance.append(x, y)
}

trait Scalaz2SpireMonoid[F] extends Any with Scalaz2SpireSemigroup[F] with spire.algebra.Monoid[F] {
  def instance: scalaz.Monoid[F]
  def id: F = instance.zero
}

trait Scalaz2Spire0 {
  implicit def scalazSemigroup2Spire[F](implicit m: scalaz.Semigroup[F]): spire.algebra.Semigroup[F] = new Scalaz2SpireSemigroup[F] {
    def instance = m
  }
}

trait Scalaz2Spire extends Scalaz2Spire0 {
  implicit def scalazMonoid2Spire[F](implicit m: scalaz.Monoid[F]): spire.algebra.Monoid[F] = new Scalaz2SpireMonoid[F] {
    def instance = m
  }
}
