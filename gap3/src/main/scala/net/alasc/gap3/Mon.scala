package net.alasc.gap3

import cyclo.Cyclo
import scalin.immutable.Vec

import net.alasc.perms.Perm

case class Mon(perm: Perm, diag: Vec[Cyclo])
