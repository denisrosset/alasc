package net.alasc

case class SiftResult[F <: FiniteElement[F]](val transversalIndices: List[Dom], val remaining: F) {
  def prepend(b: Dom) = SiftResult(b :: transversalIndices, remaining)
}

trait GroupBSGSSifting[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>
  trait BSGSSifting {
    self: BSGSChain =>

    def contains(f: F): Boolean = basicSift(f).remaining.isIdentity

    def basicSift(f: F): SiftResult[F] = this.isTerminal match {
      case true => SiftResult(Nil, f)
      case false => {
        val b = action(f, beta)
        if (!transversal.isDefinedAt(b))
          SiftResult(Nil, f)
        else {
          val nextF = f * transversal(b).uinv
          val SiftResult(transversalIndices, remaining) = tail.basicSift(nextF)
          SiftResult(b :: transversalIndices, remaining)
        }
      }
    }
  }
}
