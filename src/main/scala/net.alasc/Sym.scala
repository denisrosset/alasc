package net.alasc

import scala.util.Random

class Sym(val degree: Int) extends PermGroup[Perm] {
//  def toTeX = TeX("{S_"+degree+"}")
  override def toString = "S" + degree
  def identity = Perm(degree)
  def order = (1 to degree).foldLeft(BigInt(1))(_*_)
  def compatible(p: Perm) = p.size == degree
  def contains(p: Perm) = {
    require_(compatible(p))
    true
  }
  def randomElement(gen: Random) = new Perm(gen.shuffle((0 until degree).toBuffer).toArray)
  def elements = (0 until degree).toArray.permutations.map(new Perm(_))
  def generators = (0 to degree - 2).map(k => identity.withSwap(Dom._0(k), Dom._0(k+1)))
  def fromExplicit(p: Perm) = if (p.size == degree) Some(p) else None
}

object Sym {
  def apply(degree: Int) = new Sym(degree)
}
