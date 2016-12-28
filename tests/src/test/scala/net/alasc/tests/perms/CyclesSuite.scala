package net.alasc.tests
package perms

import net.alasc.laws.{AnyRefLaws, PermutationActionLaws, Permutations}
import net.alasc.perms.Cycles

class CyclesSuite extends AlascSuite {

  import Permutations._

  checkAll("Cycles", PermutationActionLaws[Cycles].faithfulPermutationAction)
  checkAll("Cycles", AnyRefLaws[Cycles]._eq)

}
