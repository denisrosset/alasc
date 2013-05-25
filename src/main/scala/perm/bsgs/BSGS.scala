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
  private[bsgs] def id: E

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

private[bsgs] class BSGSConstruction[E <: PermElement[E]](
  var trv: TransLike[E],
  private[bsgs] var sgList: List[E],
  val id: E,
  private[bsgs] var next: BSGSConstruction[E]) extends BSGSLike[E] {

  def putInOrder: Boolean = {
    if(next ne null) while(next.putInOrder) { }
    for (b <- trv.keysIterator) {
      val ub = trv.u(b)
      for (x <- sgList) { // TODO: test if generator is trivial with more clever transversals
        if (!trv.isDefinedAt(x.image(b)))
          trv = trv.updated(List(x), sgList)
        val schreierGen = ub*x*trv.uinv(x.image(b))
        addElement(schreierGen).map( someH => {
          if(next ne null) while(next.putInOrder) { }
          addStrongGenerator(someH)
          return true
        } )
      }
    }
    return false
  }

  def addStrongGeneratorsInChain(h: List[E]) {
    sgList = h ++ sgList
    trv = trv.updated(h, sgList)
    nextNotNullOr(next.addStrongGeneratorsInChain(h.filter(_.image(trv.beta) == trv.beta)), Unit)
  }

  def addStrongGenerator(h: E) {
    sgList = h :: sgList
    trv = trv.updated(List(h), sgList)
  }

  def addElement(el: E): Option[E] = {
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
      val newTrans = trv.builder.empty(Dom._0(newBase), id)
      next = new BSGSConstruction(newTrans, Nil, id, null)
      next.addStrongGenerator(h)
      addStrongGenerator(h)
      return Some(h)
    } else {
      next.addElement(h) match {
        case None => return None
        case Some(gen) => {
          addStrongGenerator(gen)
          return Some(gen)
        }
      }
    }
  }

  def asGroup: BSGSGroup[E] = BSGSGroup(trv, sgList, id, nextNotNullOr(next.asGroup, null))
}

object BSGSConstruction {
  def fromBaseAndGeneratingSet[E <: PermElement[E]](base: Base, genSet: List[E], id: E,
    transBuilder: TransBuilderLike = ExpTransBuilder): BSGSConstruction[E] = {

    def create(beta: Dom, tailBase: List[Dom]) = {
      var trv = transBuilder.empty(beta, id)
      trv = trv.updated(genSet, genSet)
      new BSGSConstruction(trv, genSet, id,
        fromBaseAndGeneratingSet(tailBase, genSet.filter(_.image(beta) == beta), id, transBuilder))
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

  def fromBase[E <: PermElement[E]](base: Base, id: E,
    transBuilder: TransBuilderLike = ExpTransBuilder): BSGSConstruction[E] = {

    def create(levelBase: Base): BSGSConstruction[E] = levelBase match {
      case Nil => null
      case hd :: tl => new BSGSConstruction(transBuilder.empty(hd, id), Nil, id, create(tl))
    }
    if (base.isEmpty)
      create(List(Dom._0(0)))
    else
      create(base)
  }
}


object BSGS {
  def randomSchreierSims[E <: PermElement[E]](randomElement: => E, order: BigInt, id: E, baseStrategy: BaseStrategy = EmptyBase, transBuilder: TransBuilderLike = ExpTransBuilder) = {
    val cons = BSGSConstruction.fromBase[E](baseStrategy.get(List(randomElement)), id, transBuilder)
    while (cons.order < order)
      cons.addElement(randomElement)
    cons.asGroup
  }

  def schreierSims[E <: PermElement[E]](generators: List[E], id: E, baseStrategy: BaseStrategy = EmptyBase, transBuilder: TransBuilderLike = ExpTransBuilder) = {
    val cons = BSGSConstruction.fromBaseAndGeneratingSet(baseStrategy.get(generators), generators, id, transBuilder)
    while (cons.putInOrder) { }
    cons.asGroup
  }
}
