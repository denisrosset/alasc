package net.alasc.syntax

import net.alasc.algebra._
import scala.language.implicitConversions

trait CheckSyntax {

  implicit def checkSyntax[A: Check](a: A) = new CheckOps(a)

}

trait PermutationActionSyntax {

  implicit def permutationActionSyntax[A: PermutationAction](a: A) = new PermutationActionOps(a)

}

trait WidenSyntax {

  implicit def widenKSyntax[F[_], N](narrow: F[N]) = new WidenKOps[F, N](narrow)

  implicit def widenSyntax[N](narrow: N) = new WidenOps[N](narrow)

}

trait AllSyntax
    extends CheckSyntax
    with WidenSyntax
    with PermutationActionSyntax
