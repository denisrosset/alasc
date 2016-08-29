package net.alasc.tests
package perms

import net.alasc.domains.Domain
import net.alasc.laws.{AnyRefLaws, Doms, PermutationActionLaws, Permutations}
import net.alasc.perms.Cycles

class CyclesSuite extends AlascSuite {

  import Permutations._

  val domain = Domain(100)

  import Doms.arbDomInDomain

  checkAll("Cycles", PermutationActionLaws[Cycles](domain).faithfulPermutationAction)
  checkAll("Cycles", AnyRefLaws[Cycles]._eq)

}
