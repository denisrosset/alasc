package net.alasc.laws

import spire.algebra.{GroupAction, Eq}

case class Dom(value: Int) {
  require(value >= 0)
}

object Dom {
  implicit def domToInt(d: Dom): Int = d.value
  implicit def intToDom(k: Int): Dom = apply(k)
  implicit object Eq extends Eq[Dom] {
    def eqv(x: Dom, y: Dom) = x.value == y.value
  }
  implicit def convert[A](implicit pa: GroupAction[Int, A]): GroupAction[Dom, A] = new GroupAction[Dom, A] {
    def actr(k: Dom, a: A): Dom = pa.actr(k, a)
    def actl(a: A, k: Dom): Dom = pa.actl(a, k)
  }
}
