package net.alasc

import scala.util.Random
import scala.math.{min, max}

class GroupFromGenerators[F <: FiniteElement[F]](
  identity: F,
  action: Action[F],
  generators: List[F],
  base: List[Dom] = Nil) extends Group(identity, action) {
  lazy val bsgs = BSGSChain.schreierSims(base, generators)
}

class GroupFromGeneratorsAndOrder[F <: FiniteElement[F]](
  identity: F,
  action: Action[F],
  generators: List[F],
  order: BigInt,
  base: List[Dom] = Nil) extends Group(identity, action) {
  lazy val bsgs = {
    val bag = RandomBag(generators, identity, max(10, generators.length), 50, random)
    BSGSChain.randomSchreierSims(base, bag.randomElement, order)
  }
}

class GroupFromRandomElementsAndOrder[F <: FiniteElement[F]](
  identity: F,
  action: Action[F],
  randomElement: Random => F,
  order: BigInt,
  base: List[Dom] = Nil) extends Group(identity, action) {
  lazy val bsgs = BSGSChain.randomSchreierSims(base, randomElement, order)
}

class GroupFromBSGS[F <: FiniteElement[F]](
  identity: F,
  action: Action[F],
  base: List[Dom],
  strongGeneratingSet: List[F]) extends Group(identity, action) {
  val bsgs = {
    val chain = BSGSChain.mutableFromBaseAndGeneratingSet(base, strongGeneratingSet)
    chain.makeImmutable
    chain
  }
}

