package net.alasc.print

final class NoImplicit2[A, B]

object NoImplicit2 {
  implicit def noImplicit0[A, B]: NoImplicit2[A, B] = new NoImplicit2[A, B]
  implicit def noImplicit1[A, B](implicit ev: A): NoImplicit2[A, B] = new NoImplicit2[A, B]
  implicit def noImplicit2[A, B](implicit ev: B): NoImplicit2[A, B] = new NoImplicit2[A, B]
}
