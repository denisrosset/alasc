package net.alasc

import scala.util.Random
import scala.math.{min, max}

class GroupFromGenerators[F <: FiniteElement[F]](
  identity: F,
  action: Action[F],
  generators: List[F],
  base: List[Dom] = Nil,
  options: GroupOptions = GroupOptions.default) extends Group(identity, action, options) {
  lazy val bsgs = BSGSChain.deterministicSchreierSims(base, generators)
}

class GroupFromGeneratorsAndOrder[F <: FiniteElement[F]](
  identity: F,
  action: Action[F],
  generators: List[F],
  order: BigInt,
  base: List[Dom] = Nil,
  options: GroupOptions = GroupOptions.default) extends Group(identity, action, options) {
  lazy val bsgs = options.useRandomizedAlgorithms match {
    case true => {
      val bag = RandomBag(generators, identity, max(10, generators.length), 50, options.randomGenerator)
      BSGSChain.randomSchreierSims(base, bag.randomElement, order)
    }
    case false => BSGSChain.deterministicSchreierSims(base, generators)
  }
}

class GroupFromRandomElementsAndOrder[F <: FiniteElement[F]](
  identity: F,
  action: Action[F],
  randomElement: Random => F,
  order: BigInt,
  base: List[Dom] = Nil,
  options: GroupOptions = GroupOptions.default) extends Group(identity, action, options) {
  require_(options.useRandomizedAlgorithms == true)
  lazy val bsgs = BSGSChain.randomSchreierSims(base, randomElement, order)
}

class GroupFromBSGS[F <: FiniteElement[F]](
  identity: F,
  action: Action[F],
  base: List[Dom],
  strongGeneratingSet: List[F],
  options: GroupOptions = GroupOptions.default) extends Group(identity, action, options) {
  val bsgs = {
    val chain = BSGSChain.mutableFromBaseAndGeneratingSet(base, strongGeneratingSet)
    chain.makeImmutable
    chain
  }
}

