package net.alasc.std

import spire.algebra._
import net.alasc.algebra._
import scala.collection.SeqLike
import scala.language.higherKinds

class SeqIndex[SA <: SeqLike[A, SA], A] extends Index[SA, A] {
  def length(s: SA) = s.length
  def element(s: SA, i: Int): A = s(i)
  def toIndexedSeq(s: SA): IndexedSeq[A] = s.toIndexedSeq
}

trait SeqInstances0 {
  implicit def SeqLength[CC[A] <: SeqLike[A, CC[A]], A] = new SeqIndex[CC[A], A]
}

trait SeqInstances extends SeqInstances0 {

}
