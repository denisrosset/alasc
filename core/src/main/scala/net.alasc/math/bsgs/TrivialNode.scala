package net.alasc.math
package bsgs

import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.reflect.ClassTag

import scala.collection
import scala.collection.immutable
import scala.collection.mutable
import scala.util.Random

import spire.algebra.Group
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.monoid._

case class TrivialNode[P](beta: Int, id: P, next: Chain[P])(implicit val action: FaithfulPermutationAction[P], ct: ClassTag[P]) extends Node[P] {
  def isImmutable = true
  def isMutable = false
  def foreachOrbit(f: Int => Unit) = f(beta)
  def foreachU(f: P => Unit): Unit = f(id)
  def inOrbit(b: Int) = b == beta
  def isStandalone = false
  def orbitIterator = Iterator(beta)
  def orbitSize = 1
  def nOwnGenerators = 0
  def ownGenerator(i: Int): P = Seq.empty[P].head
  def ownGeneratorInv(i: Int): P = Seq.empty[P].head
  def randomU(rand: Random) = id
  def u(b: Int) = if (b == beta) id else sys.error("Not in orbit")
  def uInv(b: Int) = if (b == beta) id else sys.error("Not in orbit")
}
