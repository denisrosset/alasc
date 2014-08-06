package net.alasc.std

import spire.algebra._
import net.alasc.algebra._
import scala.collection.SeqLike
import scala.language.higherKinds

class SeqIndex[A, SA <: SeqLike[A, SA]] extends Index[A, SA] {
  def length(s: SA) = s.length
  def element(s: SA, i: Int): A = s(i)
  def toIndexedSeq(s: SA): IndexedSeq[A] = s.toIndexedSeq
}

trait SeqInstances0 {
  implicit def SeqLength[A, CC[A] <: SeqLike[A, CC[A]]] = new SeqIndex[A, CC[A]]
}

trait SeqInstances extends SeqInstances0 {

}
