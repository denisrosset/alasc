package com.faacets
package perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions

trait BaseStrategy {
  def get(generators: List[PermElementLike]): List[Dom]
}
case class Prescribedbase(base: List[Dom]) extends BaseStrategy {
  def get(generators: List[PermElementLike]) = base
}

object EmptyBase extends BaseStrategy {
  def get(elements: List[PermElementLike]): List[Dom] = {
    val g = elements.head
    for (i <- 0 until g.size)
      if (g.image(Dom._0(i)) != Dom._0(i))
        return List(Dom._0(i))
    throw new IllegalArgumentException("The generator list should not contain the identity element.")
  }
}

object FullBase extends BaseStrategy {
  def get(elements: List[PermElementLike]) = {
    val n = elements.head.size
    (0 until n).toList.map(Dom._0(_))
  }
}

object BSGS {
  def randomSchreierSims[T <: Transversal[T, E], E <: PermElement[E]](baseStrategy: BaseStrategy, trvFactory: TransversalFactory[T, E])(randomElement: => E, id: E, order: BigInt) = {
    val cons = BSGSConstruction.fromBase[T, E](baseStrategy.get(List(randomElement)), id, trvFactory)
    while (cons.order < order)
      cons.construct(randomElement, id)
    cons.asGroup
  }
}

private[bsgs] trait BSGSLike[E <: PermElement[E]] {
  def trv: TransversalLike[E]
  private[bsgs] def sgList: List[E]
  private[bsgs] def next: BSGSLike[E]
  def nextNotNullOr[Result](f: => Result, inCaseOfNull: Result): Result = next match {
    case null => inCaseOfNull
    case _ => f
  }

  @tailrec final def length(acc: Int = 1): Int = next match {
    case null => acc
    case _ => next.length(acc + 1)
  }
  def contains(el: E) = basicSift(el)._2.isIdentity
  def uSize: List[Int] = nextNotNullOr(trv.size :: next.uSize, trv.size :: Nil)
  def order: BigInt = nextNotNullOr(trv.size * next.order, trv.size)

  def basicSift[F <: PermElement[F]](el: F)(implicit conv: E => F): (List[Dom], F) = {
    val b = el.image(trv.beta)
    if (!trv.isDefinedAt(b))
      return (Nil, el)
    val nextEl = el * trv.uinv(b)
    next match {
      case null => (b :: Nil, el)
      case _ => {
        val (bList, retEl) = next.basicSift(nextEl)
        (b :: bList, retEl)
      }
    }
  }

  def base: List[Dom] = trv.beta :: nextNotNullOr(next.base, Nil)

  def transversalSizes: List[Int] = trv.size :: nextNotNullOr(next.transversalSizes, Nil)
}
object BSGSConstruction {
    def fromBase[T <: Transversal[T, E], E <: PermElement[E]](base: Base, id: E, trvFactory: TransversalFactory[T, E]): BSGSConstruction[T, E] = {
      def create(levelBase: Base): BSGSConstruction[T, E] = levelBase match {
        case Nil => null
        case hd :: tl => new BSGSConstruction(trvFactory.empty(hd, id), Nil, create(tl))(trvFactory)
      }
      if (base.isEmpty)
        create(List(Dom._0(0)))
      else
        create(base)
    }

}
private[bsgs] class BSGSConstruction[T <: Transversal[T, E], E <: PermElement[E]](
  var trv: T,
  private[bsgs] var sgList: List[E],
  private[bsgs] var next: BSGSConstruction[T, E])(implicit trvFactory: TransversalFactory[T, E])  extends BSGSLike[E] {

  def addStrongGenerator(h: E) {
    sgList = h :: sgList
    trv = trv.addingGenerator(h)
  }

  def construct(el: E, id: E): Option[E] = {
    val b = el.image(trv.beta)
    if (!trv.isDefinedAt(b)) {
      addStrongGenerator(el)
      return Some(el)
    }
    val h = el * trv.uinv(b)
    assert(h.image(trv.beta) == trv.beta)
    if (next eq null) {
      if (h.isIdentity)
        return None
      val newBase = (0 until el.size).find( k => h.image(Dom._0(k)) != Dom._0(k) ).get
      val newTransversal = trvFactory.empty(Dom._0(newBase), id)
      next = new BSGSConstruction(newTransversal, Nil, null)
      next.addStrongGenerator(h)
      addStrongGenerator(h)
      return Some(h)
    } else {
      next.construct(h, id) match {
        case None => return None
        case Some(gen) => {
          addStrongGenerator(gen)
          return Some(gen)
        }
      }
    }
  }

  def asGroup: BSGSGroup[E] = BSGSGroup(trv, sgList, nextNotNullOr(next.asGroup, null))
}


case class BSGSGroup[E <: PermElement[E]](
  val trv: TransversalLike[E],
  private[bsgs] val sgList: List[E],
  private[bsgs] val next: BSGSGroup[E]) extends BSGSLike[E] with PermGroup[BSGSElement[E]] {
  def mapElements[F <: PermElement[F]](f: E => F): BSGSGroup[F] = new BSGSGroup[F](
    trv.mapValues(f), sgList.map(f(_)), nextNotNullOr(next.mapElements(f), null)
  )
  def size: Int = nextNotNullOr(next.size, 1) + 1
  def nextInChain: Option[BSGSGroup[E]] = nextNotNullOr(Some(next), None)
  def compatible(e: BSGSElement[E]) = (e.g == this) && nextNotNullOr(next.compatible(e.nextEl), true)
  def elements = for {
    b <- trv.keysIterator
    ne <- nextNotNullOr(next.elements, List(null).iterator)
  } yield BSGSElement(b, ne, this)
  def contains(e: BSGSElement[E]) = trv.isDefinedAt(e.b) && nextNotNullOr(next.contains(e.nextEl), true)
  def fromBaseImages(baseImages: List[Dom]): BSGSElement[E] =
    BSGSElement(baseImages.head, nextNotNullOr(next.fromBaseImages(baseImages.tail), null), this)

  def generators = sgList.iterator.map(sift(_)._1)

  def identity = BSGSElement(trv.beta, nextNotNullOr(next.identity, null), this)

  def random(implicit gen: scala.util.Random) =
    BSGSElement(trv.random(gen)._1, nextNotNullOr(next.random(gen), null), this)

  def degree = sgList.head.size

  def fromExplicit(p: Perm): Option[BSGSElement[E]] = {
    implicit def conversion(e: E) = e.explicit
    val (baseImages, remaining) = basicSift(p)
    if (remaining.isIdentity)
      Some(fromBaseImages(baseImages))
    else
      None
  }

  def sift(e: E): (BSGSElement[E], E) = {
    val (baseImages, remaining) = basicSift(e)
    (fromBaseImages(baseImages), remaining)
  }

  def toTeX = TeX("\\text{BSGS}^{" + order + "," + degree + "}_{\\begin{array}{" + "c"*size + "}" + base.mkString(" & ") + "\\\\" + transversalSizes.mkString(" & ") + "\\end{array}}")

  def removingTrivialBaseElements: BSGSGroup[E] = {
    if (trv.size == 1)
      nextNotNullOr(next.removingTrivialBaseElements, null)
    else
      BSGSGroup[E](trv, sgList, nextNotNullOr(next.removingTrivialBaseElements, null))
  }
}

case class BSGSElement[E <: PermElement[E]](b: Dom, private[bsgs] nextEl: BSGSElement[E], g: BSGSGroup[E]) extends PermElement[BSGSElement[E]] {
//  def *(that: BSGSElement[E]) = g.sift(represents * that.represents)._1 // TODO: make faster
  def *(that: BSGSElement[E]) = BSGSElement(that.image(b), nextElNotNullOr(nextEl*that.nextEl, null), g)

  def inverse = g.sift(represents.inverse)._1
  def explicit = represents.explicit
  def isIdentity = (b == g.trv.beta) && nextElNotNullOr(nextEl.isIdentity, true)
  def compatible(that: BSGSElement[E]) = g.compatible(that)
  def size = g.trv(b)._1.size
  def compare(that: BSGSElement[E]): Int = represents.compare(that.represents)
  def equal(that: BSGSElement[E]): Boolean = (b == that.b) && nextElNotNullOr( nextEl.equal(that.nextEl), true)
  def image(k: Dom) = represents.image(k)
  def invImage(k: Dom) = represents.invImage(k)
  def images0 = represents.images0
  def images1 = represents.images1
  def represents: E = nextElNotNullOr(nextEl.represents * g.trv.u(b), g.trv.u(b))
  def nextElNotNullOr[R](f: => R, v: R) = nextEl match {
    case null => v
    case _ => f
  }
  def baseImage: List[Dom] = b :: nextElNotNullOr(nextEl.baseImage, Nil)
  override def toString = "BSGS element with base image " + baseImage.mkString("(",",",")")
}
