package net.alasc.syntax

import scala.language.implicitConversions

import spire.algebra.Group

import net.alasc.algebra._


trait CheckSyntax {

  implicit def checkSyntax[A: Check](a: A) = new CheckOps(a)

}

trait PermutationActionSyntax {

  implicit def permutationActionSyntax[A: PermutationAction](a: A) = new PermutationActionOps(a)

}

trait GroupSyntax {

  implicit def groupSyntax[A: Group](a: A) = new GroupOps(a)

  implicit def richGroupSyntax[A](ev: Group[A]) = new RichGroupOps(ev)

}

trait AllSyntax
    extends CheckSyntax
    with GroupSyntax
    with PermutationActionSyntax
