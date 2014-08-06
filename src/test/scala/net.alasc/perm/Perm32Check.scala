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

class Perm32Check extends PermutationCheck[Perm32] with PermutationGenerators[Perm32] {
  implicit def algebra = Perm32.Algebra
}
