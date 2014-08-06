package net.alasc.math
package perm

import net.alasc.algebra._

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.eq._

class Perm16ValCheck extends PermutationCheck[Perm16Val] with PermutationGenerators[Perm16Val]{
  implicit def algebra = Perm16Val.Algebra
}
