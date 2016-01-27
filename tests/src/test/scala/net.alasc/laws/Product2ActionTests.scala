package net.alasc.laws

import scala.reflect.ClassTag

import spire.algebra._
import spire.algebra.lattice._
import spire.math._
import spire.laws._
import spire.std.tuples._

import scala.{ specialized => spec }

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Gen, Arbitrary}
import spire.std.int.IntAlgebra

import org.scalatest.FunSuite

import net.alasc.algebra._
import net.alasc.domains._
import net.alasc.finite._
import net.alasc.perms._
import net.alasc.prep._
import net.alasc.std.product._

class Product2ActionTests extends FunSuite with NestedDiscipline {

  import Permutations.arbDom

  implicit def permTupleArbitrary: Arbitrary[(Perm, Perm)] =
    Arbitrary( for {
      g1 <- Permutations.forSize[Perm](16)
      g2 <- Permutations.forSize[Perm](5)
    } yield (g1, g2) )

  implicit def permPermAction: FaithfulPermutationAction[(Perm, Perm)] = implicitly[PRepBuilder[(Perm, Perm)]].build(Seq((Perm(0, 15), Perm(0, 4)))).permutationAction

  checkAll("(Perm, Perm)",      PermutationActionLaws[(Perm, Perm)].faithfulPermutationAction)

}
