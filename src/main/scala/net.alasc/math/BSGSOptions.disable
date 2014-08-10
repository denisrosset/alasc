package net.alasc.math

import scala.language.implicitConversions
import scala.util.Random
import bsgs._

trait BSGSOptions {
  def randomizedOrDeterministic: RandomizedOrDeterministic
  def randomGenerator: Random
  def transversalBuilder: TransversalBuilder
  def baseChangeStrategy: BaseChangeStrategy
}

object BSGSOptions {
  implicit def toRandomizedOrDeterministic(options: BSGSOptions): RandomizedOrDeterministic = options.randomizedOrDeterministic
  implicit def toRandomGenerator(options: BSGSOptions): Random = options.randomGenerator
  implicit def toTransversalBuilder(options: BSGSOptions): TransversalBuilder = options.transversalBuilder
  implicit def toBaseChangeStrategy(options: BSGSOptions): BaseChangeStrategy = options.baseChangeStrategy

  def optimized(rg: Random = Random): BSGSOptions = new BSGSOptions {
    def randomizedOrDeterministic = Randomized
    def randomGenerator = rg
    def transversalBuilder = TransversalExplicit
    def baseChangeStrategy = BaseHeuristic
  }

  def deterministic: BSGSOptions = new BSGSOptions {
    def randomizedOrDeterministic = Deterministic
    def randomGenerator = sys.error("Randomized methods were explicitly forbidden by user")
    def transversalBuilder = TransversalExplicit
    def baseChangeStrategy = BaseHeuristic
  }
}
