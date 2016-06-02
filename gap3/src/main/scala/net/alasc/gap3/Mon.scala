package net.alasc.gap3

import cyclo.Cyclo
import spire.syntax.action._
import spire.syntax.eq._
import spire.std.int._
import scalin.immutable.{Mat, Vec}
import scalin.syntax.all._
import scalin.immutable.dense._ // TODO: use sparse when available

import net.alasc.perms.Perm

case class Mon(perm: Perm, diag: Vec[Cyclo]) {

  def dimension = diag.length

  def mat = permMat[Cyclo](perm, diag.length) * diag.toDiagMat

}
