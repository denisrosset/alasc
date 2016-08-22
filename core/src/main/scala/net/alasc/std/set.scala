package net.alasc.std

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.{SeqLike, Set, SetLike}

import spire.algebra._
import spire.std.SeqVectorEq
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._

class SeqVectorOrder[A: Order, SA <: SeqLike[A, SA]](implicit scalar: AdditiveMonoid[A])
  extends SeqVectorEq[A, SA] with Order[SA] with Serializable {
  override def eqv(x: SA, y: SA): Boolean = super[SeqVectorEq].eqv(x, y)

  def compare(x: SA, y: SA): Int = {
    @tailrec
    def loop(xi: Iterator[A], yi: Iterator[A]): Int = {
      if (xi.hasNext && yi.hasNext) {
        val cmp = Order[A].compare(xi.next(), yi.next())
        if (cmp == 0) loop(xi, yi) else cmp
      } else if (xi.hasNext) {
        if (Order[A].eqv(xi.next(), scalar.zero)) loop(xi, yi) else 1
      } else if (yi.hasNext) {
        if (Order[A].eqv(yi.next(), scalar.zero)) loop(xi, yi) else -1
      } else {
        0
      }
    }

    loop(x.toIterator, y.toIterator)
  }
}

class SetIntPermutationAction[S <: SetLike[Int, S] with Set[Int], P:Group:PermutationAction](
  implicit cbf: CanBuildFrom[Nothing, Int, S]) extends Action[S, P] {

  def actr(s: S, p: P): S = {
    val b = cbf()
    s.foreach { i: Int =>
      b += i <|+| p
    }
    b.result
  }

  def actl(p: P, s: S): S = actr(s, p.inverse)
}

trait SetInstances0 {
  implicit def SetIntPermutationAction[S <: SetLike[Int, S] with Set[Int], P:Group:PermutationAction](
    implicit cbf: CanBuildFrom[Nothing, Int, S]): Action[S, P] = new SetIntPermutationAction[S, P]
}

trait SetInstances extends SetInstances0 {

}
