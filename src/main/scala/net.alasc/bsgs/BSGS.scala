package net.alasc
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds


object BSGS {
  def apply[G <: PermGroup[E], E <: PermElement[E]](g: G)(implicit gen: Random) = {
    BSGS.randomSchreierSims(g.random(_), g.order, g.identity)
  }
  def randomSchreierSims[E <: PermElement[E]](randomElement: Random => E, order: BigInt, id: E, baseStrategy: BaseStrategy = EmptyBase, transBuilder: TransBuilderLike = ExpTransBuilder)(implicit gen: Random) = {
    val cons = BSGS.mutableFromBase[E](baseStrategy.get(List(randomElement(gen))), id, transBuilder)
    while (cons.order < order)
      cons.addElement(randomElement(gen))
    cons.makeImmutable
    cons
  }

  def schreierSims[E <: PermElement[E]](generators: List[E], id: E, baseStrategy: BaseStrategy = EmptyBase, transBuilder: TransBuilderLike = ExpTransBuilder) = {
    val cons = BSGS.mutableFromBaseAndGeneratingSet(baseStrategy.get(generators), generators, id, transBuilder)
    while (cons.putInOrder) { }
    cons.removeRedundantGenerators
    cons.makeImmutable
    cons
  }

  def fromBaseAndStrongGeneratingSet[E <: PermElement[E]](base: Base, strongGeneratingSet: List[E], identity: E, transBuilder: TransBuilderLike = ExpTransBuilder): BSGSGroup[E] = {
    val cons = mutableFromBaseAndGeneratingSet(base, strongGeneratingSet, identity, transBuilder)
    cons.makeImmutable
    cons
  }
  // Internal constructions
  private[bsgs] def mutableFromBaseAndGeneratingSet[E <: PermElement[E]](base: Base, genSet: List[E], id: E,
    transBuilder: TransBuilderLike = ExpTransBuilder): BSGSGroup[E] = {
    def create(beta: Dom, tailBase: Base) = {
      var trv = transBuilder.empty(beta, id)
      trv = trv.updated(genSet, genSet)
      new BSGSGroupNode(trv, genSet, id, false,
        mutableFromBaseAndGeneratingSet(tailBase, genSet.filter(_.image(beta) == beta), id, transBuilder))
    }
    base.list match {
      case Nil => {
        val genNotIdentity = genSet.filter(!_.isIdentity)
        if (genNotIdentity.isEmpty)
          return BSGSGroupTerminal(id)
        else {
          for (g <- genNotIdentity; i <- 0 until g.size; k = Dom._0(i) if g.image(k) != k)
            return create(k, Base(Nil))
          throw new IllegalArgumentException("Bad arguments.")
        }
      }
      case hd :: tl => create(hd, Base(tl))
    }
  }

  private[bsgs] def mutableFromBase[E <: PermElement[E]](base: Base, id: E,
    transBuilder: TransBuilderLike = ExpTransBuilder): BSGSGroup[E] = {

    def create(levelBase: Base): BSGSGroup[E] = levelBase.list match {
      case Nil => BSGSGroupTerminal(id)
      case hd :: tl => new BSGSGroupNode(transBuilder.empty(hd, id), Nil, id, false, create(Base(tl)))
    }
    if (base.list.isEmpty)
      create(Base(List(Dom._0(0))))
    else
      create(base)
  }
}
