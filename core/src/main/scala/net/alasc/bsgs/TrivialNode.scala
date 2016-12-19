package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.Group

import net.alasc.algebra._

case class TrivialNode[G:ClassTag:Group, A <: PermutationAction[G] with Singleton](beta: Int, next: Chain[G, A])
                                                            (implicit val action: A) extends Node[G, A] {

  def elements = Iterable(Group[G].id)
  def elementFor(g: G) = Group[G].id

  def isImmutable = true
  def isMutable = false

  def foreachOrbit(f: Int => Unit) = f(beta)
  def foreachU(f: G => Unit): Unit = f(Group[G].id)

  def inOrbit(b: Int) = b == beta
  def isStandalone = false

  def orbitIterator = Iterator(beta)
  def orbitSize = 1

  def nOwnGenerators = 0
  def ownGenerator(i: Int): G = Seq.empty[G].head
  def ownGeneratorInv(i: Int): G = Seq.empty[G].head

  def randomU(rand: Random) = Group[G].id
  def u(b: Int) = if (b == beta) Group[G].id else sys.error("Not in orbit")
  def uInv(b: Int) = if (b == beta) Group[G].id else sys.error("Not in orbit")

}
