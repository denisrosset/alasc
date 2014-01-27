package net.alasc

trait FiniteImplicits {
  implicit class RichFinite[F <: Finite[F]](val f: F) {
/*
Given an element \\( e \\), and \\( n \\) elements `generators` \\( = g_1, ..., g_n \\), 
associated with a `cost` function, finds heuristically an element
\\( e' = g_k g_l ... e g_m g_n ... \\) such that \\( e' \\) has minimal cost.

The algorithm is started with `forms = Seq(e)` and `remainingIterations = iterations`, and
each iteration does the following:

- we define `m` as the minimum of `maximumForms` and
  \\( \left \lfloor \frac{\verbatim{maximumProducts}}{2 n} \right \rfloor  \\),
- we form all the products \\( g_j f \\) and \\( f g_j \\), where \\( f \\) is an element
  of `forms` and \\( g_j \\) an element of `generators`,
- from the original sequence `forms` and these new products, we keep the \\( m \\) minimal elements,
  which are used in the next iteration of the algorithm.

At the last iteration, we return the form with minimal cost.
*/
    def minimalForm(generators: Seq[F],
      cost: F => Int,
      maximumProducts: Int = 10000,
      maximumForms: Int = 100,
      iterations: Int = 3) = {
      val m: Int = math.min(maximumForms, maximumProducts / (2 * generators.size))
      findMinimal(Seq(f), generators, cost, m, iterations)
    }
    @annotation.tailrec
    private def findMinimal(forms: Seq[F], generators: Seq[F], cost: F => Int, m: Int,
      remainingIterations: Int): F = {
      import collection.mutable.SortedSet
      val orderByCost = Ordering[Int].on[F](e => cost(e))
      remainingIterations match {
        case 0 => forms.min(orderByCost)
        case _ => {
          val set = SortedSet(forms:_*)(orderByCost)
          for (f <- forms; g <- generators) {
            set += f * g
            set += g * f
            while (set.size > m) {
              set -= set.max(orderByCost)
            }
          }
          findMinimal(set.toSeq, generators, cost, m, remainingIterations - 1)
        }
      }
    }
  }
}
