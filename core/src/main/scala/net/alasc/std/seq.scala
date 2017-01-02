package net.alasc.std

import scala.language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

import spire.algebra._
import spire.algebra.partial._
import spire.syntax.cfor._
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._

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
