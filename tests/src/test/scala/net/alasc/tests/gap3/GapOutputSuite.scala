package net.alasc.tests
package gap3

import spire.math.SafeLong

import net.alasc.gap3.GapOutput

class GapOutputSuite extends AlascSuite {
// TODO: add ~ End to all; find a solution to avoid the erasure warnings
  import fastparse.noApi._
  import GapOutput.White._

  test("Cycles without whitespace") {
    val Parsed.Success(_, _) = (GapOutput.cycles ~ End).parse("(1,2,3)(1,2)")
  }

  test("Cycles with whitespace") {
    val Parsed.Success(_, _) = (GapOutput.cycles ~ End).parse("(1,2 ,3 ) ( 1 , 2)")
  }

  test("GroupWithGenerators") {
    val Parsed.Success(grp, _) = GapOutput.groupWithGenerators.parse("GroupWithGenerators( [ (1,4,3,6,5,8,7,2), (1,2,3,4,5,6,7,8) ] )")
    grp.order should === (SafeLong(16))
  }

  test("Parse cyclotomics") {
    val Parsed.Success(_, _) = GapOutput.cyclo.parse("1/3*E(12)^4-1/2*E(12)^7-1/2*E(12)^11")
    val Parsed.Success(_, _) = GapOutput.cyclo.parse("1/2*E(8)-1/2*E(8)^3")
    val Parsed.Success(_, _) = GapOutput.cyclo.parse("-1/2*E(8)+1/2*E(8)^3")

  }

  test("Parse vector of cyclotomics") {
    val Parsed.Success(_, _) = GapOutput.cycloVec.parse("[ 1, 1, 1, E(4) ]")
  }

  test("Parse rationals and integers as cyclotomics") {
    val Parsed.Success(_, _) = GapOutput.cyclo.parse("0")
    val Parsed.Success(_, _) = GapOutput.cyclo.parse("1")
    val Parsed.Success(_, _) = GapOutput.cyclo.parse("-1")
    val Parsed.Success(_, _) = GapOutput.cyclo.parse("-1/2")
    val Parsed.Success(_, _) = GapOutput.cyclo.parse("22/3")
  }

  test("Parse DiagonalAMat") {
    val Parsed.Success(_, _) = GapOutput.diagonalAMat.parse("DiagonalAMat([ 1, 1, 1, E(4) ])")
  }

  test("Parse AMatMat (simple)") {
    val str = "AMatMat( [ [ 1, 1 ], [ 1, 1 ] ] )"
    val Parsed.Success(_, _) = GapOutput.aMat.parse(str)
  }

  test("Parse cyclotomic vector") {
    val Parsed.Success(_, _) = GapOutput.cycloVec.parse("[ 1/2*E(8)-1/2*E(8)^3, -1/2*E(8)+1/2*E(8)^3 ]")
  }

  test("Parse AMatMat (complex)") {
    val str = """AMatMat(
                      [ [ 1/2*E(8)-1/2*E(8)^3, -1/2*E(8)+1/2*E(8)^3 ],
                      [ -1/2*E(8)+1/2*E(8)^3, -1/2*E(8)+1/2*E(8)^3 ] ]
                    )"""
    val Parsed.Success(_, _) = (GapOutput.aMat ~ End).parse(str)
  }

  test("Parse AMat products") {
    val str = "IdentityPermAMat(1) * IdentityPermAMat(1) * IdentityPermAMat(1)"
    val Parsed.Success(_, _) = GapOutput.aMat.parse(str)
  }

  test("Parse AMat") {
    val str = """AMatPerm((1,8)(2,4)(3,6)(5,7), 8) *
      TensorProductAMat(
        IdentityPermAMat(2),
        AMatPerm((2,3,4), 4) *
          TensorProductAMat(
            DFTAMat(2),
            IdentityPermAMat(2)
          ) *
          DiagonalAMat([ 1, 1, 1, E(4) ]) *
    TensorProductAMat(
      IdentityPermAMat(2),
      DFTAMat(2)
    ) *
      AMatPerm((2,3), 4)
    ) *
    AMatMon( Mon(
      (2,5,3)(4,7),
      [ 1, 1, 1, E(4), 1, 1, 1, 1 ]
    ) ) *
    DirectSumAMat(
      TensorProductAMat(
        DFTAMat(2),
        IdentityPermAMat(2)
      ),
      IdentityPermAMat(4)
    ) *
      AMatPerm((2,4)(5,7)(6,8), 8)
    """
    val Parsed.Success(_, _) = (GapOutput.aMat ~ End).parse(str)
  }
}
