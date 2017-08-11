package net.alasc.domains

import spire.util.Opt

import net.alasc.util._

class Domain private (val size: Int) {

  override def toString = s"Domain($size)"

}

object Domain extends UniquenessCache[Int, Domain] {

  val empty: Domain = Domain(0)

  protected def valueFromKey(size: Int): Domain = new Domain(size)
  protected def keyFromValue(domain: Domain): Option[Int] = Some(domain.size)

  val alphabetMap = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".zipWithIndex.toMap

}
