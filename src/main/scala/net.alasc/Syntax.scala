package net.alasc

import scala.language.implicitConversions

trait IndexSyntax {
  implicit def indexOps[T](t: T) = new IndexOps(t)
}

trait LexicoSyntax {
  implicit def lexicoFirstOps[P, F <: Finite[F]](lhs: P)(implicit lex: LexicoFirst[P, F]) = 
    new LexicoFirstOps[P, F](lhs)
  implicit def lexicoSeqOps[P, F <: Finite[F]](lhs: P)(implicit lex: LexicoSeq[P, F]) =
    new LexicoSeqOps[P, F](lhs)
}

trait AllSyntax extends
    IndexSyntax with
    LexicoSyntax
