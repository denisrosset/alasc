package net.alasc.laws

import spire.algebra.{Action, Eq}

class Dom(val value: Int) extends AnyVal

object Dom {
  def unapply(dom: Dom): Option[Int] = Some(dom.value)
  def apply(value: Int): Dom = {
    require(value >= 0)
    new Dom(value)
  }
  implicit def domToInt(d: Dom): Int = d.value
  implicit def intToDom(k: Int): Dom = apply(k)
  implicit object Eq extends Eq[Dom] {
    def eqv(x: Dom, y: Dom) = x.value == y.value
  }
  implicit def convert[A](implicit pa: Action[Int, A]): Action[Dom, A] = new Action[Dom, A] {
    def actr(k: Dom, a: A): Dom = pa.actr(k, a)
    def actl(a: A, k: Dom): Dom = pa.actl(a, k)
  }
}
