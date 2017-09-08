package net.alasc.std

import scala.collection.mutable.ArrayBuffer

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.action._

import net.alasc.algebra._

class SeqPermutationAction[A, G:PermutationAction] extends Action[Seq[A], G] {

  def actl(p: G, s: Seq[A]): Seq[A] = {
    val res = ArrayBuffer.fill[A](s.size)(null.asInstanceOf[A])
    cforRange(0 until s.length) { i =>
      res(i) = s(i <|+| p)
    }
    res
  }

  def actr(s: Seq[A], p: G): Seq[A] = {
    val res = ArrayBuffer.fill[A](s.size)(null.asInstanceOf[A])
    cforRange(0 until s.length) { i =>
      res(i <|+| p) = s(i)
    }
    res
  }

}

trait SeqInstances0 {
  implicit def IndexedSeqPermutationAction[A, G:PermutationAction]: Action[Seq[A], G] = new SeqPermutationAction[A, G]
}

trait SeqInstances extends SeqInstances0 {

}
