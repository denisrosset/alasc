package net.alasc.tests
package gap3

import spire.math.SafeLong

import fastparse.core.Parsed

import net.alasc.gap3.GapOutput

class GapOutput extends AlascSuite {

  test("GroupWithGenerators") {
    val Parsed.Success(grp, _) = GapOutput.groupWithGenerators.parse("GroupWithGenerators( [ (1,4,3,6,5,8,7,2), (1,2,3,4,5,6,7,8) ] )")
    grp.order should === (SafeLong(16))
  }

}
