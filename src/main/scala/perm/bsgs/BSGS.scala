package com.faacets
package perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds

private[bsgs] trait BSGSLike[E <: PermElement[E]] {
  def trv: TransLike[E]
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
      case null => (b :: Nil, nextEl)
      case _ => {
        val (bList, retEl) = next.basicSift(nextEl)
        (b :: bList, retEl)
      }
    }
  }

  def base: List[Dom] = trv.beta :: nextNotNullOr(next.base, Nil)

  def transversalSizes: List[Int] = trv.size :: nextNotNullOr(next.transversalSizes, Nil)
}

private[bsgs] class BSGSConstruction[T[E <: PermElement[E]] <: Trans[T[E], E], E <: PermElement[E]](
  var trv: T[E],
  private[bsgs] var sgList: List[E],
  private[bsgs] var next: BSGSConstruction[T, E])(implicit transComp: TransCompanion[T])  extends BSGSLike[E] {

  def putInOrder(id: E): Boolean = {
    if(next ne null) while(next.putInOrder(id)) { }
    for (b <- trv.keysIterator) {
      val ub = trv.u(b)
      for (x <- sgList) { // TODO: test if generator is trivial with more clever transversals
        if (!trv.isDefinedAt(x.image(b)))
          trv = trv.updated(List(x), sgList)
        val schreierGen = ub*x*trv.uinv(x.image(b))
        addElement(schreierGen, id).map( someH => {
          if(next ne null) while(next.putInOrder(id)) { }
          addStrongGenerator(someH)
          return true
        } )
      }
    }
    return false
  }

  def addStrongGenerator(h: E) {
    sgList = h :: sgList
    trv = trv.updated(List(h), h :: sgList)
  }

  def addElement(el: E, id: E): Option[E] = {
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
      val newTrans = transComp.empty(Dom._0(newBase), id)
      next = new BSGSConstruction(newTrans, Nil, null)
      next.addStrongGenerator(h)
      addStrongGenerator(h)
      return Some(h)
    } else {
      next.addElement(h, id) match {
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

object BSGSConstruction {
  def fromBaseAndGeneratingSet[
    T[E <: PermElement[E]] <: Trans[T[E], E],
    E <: PermElement[E]
  ](base: Base, genSet: List[E], id: E,
    transComp: TransCompanion[T] = ExpTransCompanion): BSGSConstruction[T, E] = {

    def create(beta: Dom, tailBase: List[Dom]) = {
      var trv = transComp.empty(beta, id)
      trv = trv.updated(genSet, genSet)
      new BSGSConstruction(trv, genSet,
        fromBaseAndGeneratingSet(tailBase, genSet.filter(_.image(beta) == beta), id, transComp))(transComp)
    }
    base match {
      case Nil => {
        val genNotIdentity = genSet.filter(!_.isIdentity)
        if (genNotIdentity.isEmpty)
          return null
        else {
          for (g <- genNotIdentity; i <- 0 until g.size; k = Dom._0(i) if g.image(k) != k)
            return create(k, Nil)
          throw new IllegalArgumentException("Bad arguments.")
        }
      }
      case hd :: tl => create(hd, tl)
    }
  }

  def fromBase[
    T[E <: PermElement[E]] <: Trans[T[E], E],
    E <: PermElement[E]](base: Base, id: E,
    transComp: TransCompanion[T] = ExpTransCompanion): BSGSConstruction[T, E] = {

    def create(levelBase: Base): BSGSConstruction[T, E] = levelBase match {
      case Nil => null
      case hd :: tl => new BSGSConstruction(transComp.empty(hd, id), Nil, create(tl))(transComp)
    }
    if (base.isEmpty)
      create(List(Dom._0(0)))
    else
      create(base)
  }
}


object BSGS {
  def randomSchreierSims[
    T[E <: PermElement[E]] <: Trans[T[E], E],
    E <: PermElement[E]](randomElement: => E, order: BigInt, id: E, baseStrategy: BaseStrategy = EmptyBase, transComp: TransCompanion[T] = ExpTransCompanion) = {
    val cons = BSGSConstruction.fromBase[T, E](baseStrategy.get(List(randomElement)), id, transComp)
    while (cons.order < order)
      cons.addElement(randomElement, id)
    cons.asGroup
  }

  def schreierSims[
    T[E <: PermElement[E]] <: Trans[T[E], E],
    E <: PermElement[E]](generators: List[E], id: E, baseStrategy: BaseStrategy = EmptyBase, transComp: TransCompanion[T] = ExpTransCompanion) = {
    val cons = BSGSConstruction.fromBaseAndGeneratingSet(baseStrategy.get(generators), generators, id, transComp)
    while (cons.putInOrder(id)) { }
    cons.asGroup
  }
}
