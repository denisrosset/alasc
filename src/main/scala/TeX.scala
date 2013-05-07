package com.faacets
class TeX(val s: String) extends AnyVal {
  def +(that: TeX): TeX = TeX(s + that.s)
}

trait HasTeX extends Any {
  def toTeX: TeX
  override def toString = toTeX
}

object TeX {
  def apply(s: String) = new TeX(s)
  def mk(seq: Seq[TeX], sep: TeX) = TeX(seq.map(_.s).mkString(sep.s))
  def mk(seq: Seq[TeX], begin: TeX, sep: TeX, end: TeX) = TeX(seq.map(_.s).mkString(begin.s, sep.s, end.s))
}
