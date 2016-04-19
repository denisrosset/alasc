package net.alasc

import scala.util.Random

import spire.util.Opt

package object perms {

  object deterministicNoSwap extends Algorithms(Opt.empty[Random], true, false)

  object deterministicNoConjugate extends Algorithms(Opt.empty[Random], false, false)

  object deterministic extends Algorithms(Opt.empty[Random], false, true)

  object default extends Algorithms(Opt(Random), false, true)


}
