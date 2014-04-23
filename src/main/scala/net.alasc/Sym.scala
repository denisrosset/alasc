package net.alasc

import scala.util.Random

class Sym private(val degree: Int) extends PermutingGroup[Perm] with PermutingGroupImpl[Perm] {
  override def toString = "S" + degree
  def identity = Perm(degree)
  def order = (1 to degree).foldLeft(BigInt(1))(_*_)
  def contains(p: Perm) = (p.size == degree)
  protected def identitySequence = (0 until degree).toBuffer
  def random(implicit gen: Random) = {
    import Dom.ZeroBased._
    val images = gen.shuffle(identitySequence)
    Perm.fromImages(degree)(k => images(k))
  }
  def elements = new Iterable[Perm] {
    import Dom.ZeroBased._
    def iterator =
      identitySequence.permutations.map(images => Perm.fromImages(degree)(k => images(k)))
  }
  def generators = {
    import Dom.ZeroBased._
    (0 to degree - 2).map(k => identity(k, k + 1))
  }
}

object Sym {
  def apply(degree: Int) = new Sym(degree)
}
