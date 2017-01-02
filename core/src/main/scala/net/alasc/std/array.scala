package net.alasc.std

import scala.reflect.ClassTag

import spire.algebra.Action
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra._

class ArrayPermutationAction[A:ClassTag, G:PermutationAction] extends Action[Array[A], G] {

  def actl(g: G, s: Array[A]): Array[A] = {
    val b = new Array[A](s.length)
    cforRange(0 until s.length) { i =>
      b(i) = s(i <|+| g)
    }
    b
  }

  def actr(s: Array[A], g: G): Array[A] = {
    val b = new Array[A](s.length)
    cforRange(0 until s.length) { i =>
      b(i <|+| g) = s(i)
    }
    b
  }
}

trait ArrayInstances0 {
  implicit def ArrayPermutationAction[A:ClassTag, G:PermutationAction]: Action[Array[A], G] = new ArrayPermutationAction[A, G]
}

trait ArrayInstances extends ArrayInstances0
