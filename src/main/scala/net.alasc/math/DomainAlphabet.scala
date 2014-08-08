package net.alasc.math

case class DomainAlphabet(map: Map[Char, Int])

object DomainAlphabet {
  def apply(str: String): DomainAlphabet = DomainAlphabet(str.zipWithIndex.toMap)
}
