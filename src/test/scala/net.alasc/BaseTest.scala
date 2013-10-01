package net.alasc

import org.scalacheck._
import scala.util.Random

object BaseGenerators {
  val groupAndBase = for {
    g <- Gen.oneOf(M11.g, M12.g, M24.g)
    n <- Gen.choose(3, 6)
    method <- Gen.oneOf(BaseSwapOnly, BaseSwapAndConjugation, BaseFromScratch)
    base <- Gen.listOfN(n, Gen.choose(1, g.identity.size)) if base.distinct.size == base.size
    options = GroupOptions.default.copy(baseChangeStrategy = method)
  } yield (g.withOptions(options), base.map(Dom._1(_)))
}

object BaseSpec extends Properties("GroupBSGSBase") {
  import BaseGenerators._

  property("After changing base and changing back, transversal orbits should be the same") = Prop.forAll(groupAndBase) {
    case (group, newBase) => {
      val start = group.bsgs
      val modified = start.withBase(newBase)
      val comingBack = modified.withBase(start.base)
      start.transversals.map(_.orbit).sameElements(comingBack.transversals.map(_.orbit))
    }
  }
}
