package net.alasc.syntax

import net.alasc.algebra._
import scala.language.implicitConversions

trait CheckSyntax {

  implicit def checkSyntax[A: Check](a: A) = new CheckOps(a)

}

trait PermutationActionSyntax {

  implicit def permutationActionSyntax[A: PermutationAction](a: A) = new PermutationActionOps(a)

}

trait AllSyntax
    extends CheckSyntax
    with PermutationActionSyntax
