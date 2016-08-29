package net.alasc.std

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.{SeqLike, Set, SetLike}

import spire.algebra._
import spire.std.SeqVectorEq
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._

class SetIntPermutationRightAction[S <: SetLike[Int, S] with Set[Int], P:PermutationAction]
  (implicit cbf: CanBuildFrom[Nothing, Int, S]) extends RightAction[S, P] {

  def actr(s: S, p: P): S = {
    val b = cbf()
    s.foreach { i: Int =>
      b += i <|+| p
    }
    b.result
  }

}


class SetIntPermutationAction[S <: SetLike[Int, S] with Set[Int], P:Group:PermutationAction]
  (implicit cbf: CanBuildFrom[Nothing, Int, S]) extends SetIntPermutationRightAction[S, P] with Action[S, P] {

  def actl(p: P, s: S): S = actr(s, p.inverse)

}

trait SetInstances0 {
  implicit def SetIntPermutationAction[S <: SetLike[Int, S] with Set[Int], P:Group:PermutationAction](
    implicit cbf: CanBuildFrom[Nothing, Int, S]): Action[S, P] = new SetIntPermutationAction[S, P]
}

trait SetInstances extends SetInstances0 {

}
