package net.alasc.prep.bsgs

import net.alasc.algebra.FaithfulPermutationAction

final class RichBaseGuideOpt(val baseGuide: BaseGuide) extends AnyVal {

  def baseAnsatz[G](generators: Iterable[G], action: FaithfulPermutationAction[G]): Seq[Int] =
    if (baseGuide eq null)
      Seq.empty[Int]
    else
      baseGuide.baseAnsatz(generators, action)

}
