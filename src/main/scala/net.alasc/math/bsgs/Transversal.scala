package net.alasc.math
package bsgs

import scala.util.Random
import scala.collection.immutable.BitSet

import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._
import net.alasc.util._

/** Contains information about a transversal in a BSGS chain. */  
trait Transversal[P] {
  def beta: Int
  def inOrbit(b: Int): Boolean
  def orbitSize: Int
  def orbitIterator: Iterator[Int]
  def foreachOrbit(f: Int => Unit): Unit
  def orbit: Iterable[Int] = new Iterable[Int] {
    override def size = orbitSize
    override def stringPrefix = "Iterable"
    override def foreach[U](f: Int => U) = foreachOrbit { k => f(k) }
    def iterator = orbitIterator
  }
  def randomOrbit(rand: Random): Int = orbit.drop(rand.nextInt(orbitSize)).head
  def orbitMin: Int = {
    var m = beta
    foreachOrbit { k =>
      if (k < m) m = k
    }
    m
  }
  def orbitMax: Int = {
    var m = beta
    foreachOrbit { k =>
      if (k > m) m = k
    }
    m
  }
  def orbitSet: Set[Int] = {
    val bitset = MutableBitSet.empty
    foreachOrbit { bitset += _ }
    bitset.toImmutable
  }
  def iterable: Iterable[(Int, InversePair[P])]
  def foreachU[N](f: P => N): Unit
  def uPair(b: Int): InversePair[P]
  def u(b: Int): P
  def uInv(b: Int): P
  def randomU(rand: Random): P
}

object Transversal {
  def empty[P](beta: Int)(implicit algebra: FiniteGroup[P]): Transversal[P] = new EmptyTransversal(beta)
}

case class ConjugatedTransversal[P](originalTransversal: Transversal[P], conjugatedBy: InversePair[P])(implicit algebra: FiniteGroup[P], action: FaithfulPermutationAction[P]) extends Transversal[P] {
  import conjugatedBy.{g, gInv}
  def orbitSize = originalTransversal.orbitSize
  def beta = originalTransversal.beta <|+| g
  def inOrbit(b: Int) = originalTransversal.inOrbit(b <|+| gInv)
  def orbitIterator = originalTransversal.orbitIterator.map(b => b <|+| g)
  def foreachOrbit(f: Int => Unit) = originalTransversal.foreachOrbit(b => f(b <|+| g))
  def iterable = originalTransversal.iterable.map {
    case (b, uIp) => (b <|+| g, conjugatedBy.inverse |+| uIp |+| conjugatedBy)
  }
  def foreachU[N](f: P => N): Unit = originalTransversal.foreachU(u => f(gInv |+| u |+| g))
  def uPair(b: Int) = conjugatedBy.inverse |+| originalTransversal.uPair(b <|+| gInv) |+| conjugatedBy
  def u(b: Int) = gInv |+| originalTransversal.u(b <|+| gInv) |+| g
  def uInv(b: Int) = g |+| originalTransversal.uInv(b <|+| gInv) |+| gInv
  def randomU(rand: Random) = gInv |+| originalTransversal.randomU(rand) |+| g
}

class EmptyTransversal[P](val beta: Int)(implicit algebra: FiniteGroup[P]) extends Transversal[P] {
  def orbitSize = 1
  def inOrbit(b: Int) = beta == b
  def orbitIterator = Iterator(beta)
  def foreachOrbit(f: Int => Unit) = { f(beta) }
  def iterable = Iterable((beta -> algebra.id))
  def foreachU[N](f: P => N) = { f(algebra.id) }
  def uPair(b: Int) = InversePair(algebra.id, algebra.id)
  def u(b: Int) = { require(b == beta); algebra.id }
  def uInv(b: Int) = u(b)
  def randomU(rand: Random) = algebra.id
}
