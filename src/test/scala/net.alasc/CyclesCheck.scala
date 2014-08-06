package net.alasc.math

import net.alasc.algebra._

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.eq._

class CyclesCheck extends PermutationCheck[Cycles] with PermutationGenerators[Cycles] {
  implicit def algebra = Cycles.Algebra
}
