/*
case class DirectSum[S](seq: Seq[S])

object DirectSum {

  implicit def DirectSumSubgroup[S, G](implicit sg: Subgroup[S, G]): Subgroup[DirectSum[S], G] =
    new DirectSumSubgroup[S, G]

}

class DirectSumSubgroup[S, G](implicit val sg: Subgroup[S, G]) extends Subgroup[DirectSum[S], G] {

  implicit def equ: Eq[G] = sg.equ
  implicit def group: Group[G] = sg.group

  type DS = DirectSum[S]
  def iterator(ds: DS) = {
    def rec(i: Int): Iterator[G] =
      if (i == ds.seq.size) Iterator(Group[G].id) else for {
        el1 <- rec(i + 1)
        el2 <- ds.seq(i).iterator
      } yield el1 |+| el2
    rec(0)
  }
  def generators(ds: DS) = ds.seq.flatMap(_.generators)
  def order(ds: DS) = (BigInt(1) /: ds.seq) { case (o, s) => o * s.order }
  def randomElement(ds: DS, gen: Random) = (Group[G].id /: ds.seq) { case (g, s) => g |+| s.randomElement(gen) }
  def contains(ds: DS, el: G) = iterator(ds).exists( g => (el === g))

}
 */
