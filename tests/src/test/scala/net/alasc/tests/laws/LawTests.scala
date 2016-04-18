package net.alasc.tests
package laws

import spire.algebra.lattice._
import spire.laws.{Perm => _, _}
import spire.std.int.IntAlgebra

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite

import net.alasc.domains._
import net.alasc.finite._
import net.alasc.laws.Partitions._
import net.alasc.laws.{AnyRefLaws, PermutationActionLaws, _}
import net.alasc.perms._

class LawTests extends FunSuite with NestedDiscipline {

  /*

  {
    nestedCheckAll[WrSize]("Wr[Perm,Perm] (imprimitive)", WrSize(1, 1)) { implicit wrSize =>
      implicit def action = wrSize.imprimitiveRepresentation[Perm, Perm].permutationAction
      PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction
    }
  }
  {
    implicit def arbWrSize = WrSize.arbWrSizeForPrimitive
    nestedCheckAll[WrSize]("Wr[Perm,Perm] (primitive)", WrSize(2, 2)) { implicit wrSize =>
      implicit def action = wrSize.primitiveRepresentation[Perm, Perm].permutationAction
      PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction
    }
  }
*/
}
