package net.alasc.math

import scala.language.implicitConversions
import scala.util.Random
import bsgs._

trait BSGSOptions {
  def algorithmType: AlgorithmType
  def randomGenerator: Random
  def mutableNodeBuilder: BSGSMutableNodeBuilder
  def baseChangeStrategy: BaseChangeStrategy
}

object BSGSOptions {
  implicit def toAlgorithmType(options: BSGSOptions): AlgorithmType = options.algorithmType
  implicit def toRandomGenerator(options: BSGSOptions): Random = options.randomGenerator
  implicit def toMutableNodeBuilder(options: BSGSOptions): BSGSMutableNodeBuilder = options.mutableNodeBuilder
  implicit def toBaseChangeStrategy(options: BSGSOptions): BaseChangeStrategy = options.baseChangeStrategy

  def userRandom(rg: Random): BSGSOptions = new BSGSOptions {
    def algorithmType = Randomized
    def randomGenerator = rg
    def mutableNodeBuilder = BSGSMutableNodeExplicit
    def baseChangeStrategy = BaseHeuristic
  }

  object deterministic {
    implicit val options: BSGSOptions = new BSGSOptions {
      def algorithmType = Deterministic
      def randomGenerator = sys.error("Randomized methods were explicitly forbidden by user")
      def mutableNodeBuilder = BSGSMutableNodeExplicit
      def baseChangeStrategy = BaseHeuristic
    }
  }

  object randomized {
    implicit val options: BSGSOptions = new BSGSOptions {
      def algorithmType = Randomized
      def randomGenerator = Random
      def mutableNodeBuilder = BSGSMutableNodeExplicit
      def baseChangeStrategy = BaseHeuristic
    }
  }
}
