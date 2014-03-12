package net.alasc

case class Cycle(seq: Seq[Dom]) {
  /** Returns a text representation of this cycle, using `Dom.displayIndex`. */
  override def toString = seq.mkString("(", ",", ")")
  /** Returns a text representation of this cycle, using the given symbols. */
  def toStringUsingSymbols(symbols: Seq[String]) = {
    import Dom.ZeroBased._
    seq.map(symbols(_)).mkString("(",",",")")
  }
}

class Cycles(val seq: Seq[Cycle]) extends FiniteLike[Cycles] {
  /** Returns a text representation of these cycles, using `Dom.displayIndex`. */
  override def toString = seq.mkString
  /** Returns a text representation of these cycles, using the given symbols. */
  def toStringUsingSymbols(symbols: Seq[String]) = seq.map(_.toStringUsingSymbols(symbols)).mkString
  lazy val minimalSize = {
    val roomyPerm = toPerm((seq.flatMap(_.seq.map(_._1)) :+ 1).max)
    (roomyPerm.size to 1 by -1).map(Dom._1).find(k => roomyPerm.image(k) != k).getOrElse(Dom._1(1))._1
  }
  lazy val toMinimalPerm = toPerm(minimalSize)
  def toPerm(size: Int): Perm = Perm(size)(this)
  def ===(that: Cycles) = {
    val thisPerm = this.toMinimalPerm
    val thatPerm = that.toMinimalPerm
    if (thisPerm.size != thatPerm.size)
      false
    else
      thisPerm === thatPerm
  }
  def apply(seq: Dom*): Cycles = this * Cycles(seq: _*)
  def *(that: Cycles) = {
    val size = scala.math.max(this.minimalSize, that.minimalSize)
    (this.toPerm(size) * that.toPerm(size)).cycles
  }
  def image(k: Dom): Dom = {
    if (k._1 <= minimalSize)
      toMinimalPerm.image(k)
    else
      k
  }
  def inverse = toMinimalPerm.inverse.cycles
  def hash = toMinimalPerm.hash
  def isIdentity = seq.isEmpty
  def dontForgetToOverrideHashCodeAndEquals = true
  override def hashCode = hash
  override def equals(any: Any) = any match {
    case that: Cycles => this == that
    case _ => false
  }
}

object Cycles {
  def apply(seq: Dom*): Cycles = new Cycles(Seq(Cycle(seq)))
}
