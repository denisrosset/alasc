package net.alasc

import scala.language.implicitConversions

trait IndexSyntax {
  implicit def indexOps[T](t: T) = new IndexOps(t)
}

trait LexicoSyntax {
  implicit def lexicoFirstOps[P: LexicoFirst](lhs: P) = new LexicoFirstOps(lhs)
  implicit def lexicoSeqOps[P: LexicoSeq](lhs: P) = new LexicoSeqOps(lhs)
}

trait AllSyntax extends
    IndexSyntax with
    LexicoSyntax
