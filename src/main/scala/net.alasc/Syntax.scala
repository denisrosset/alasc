package net.alasc

import scala.language.implicitConversions

trait ActionSyntax {
  implicit def actionOps[P](p: P) = new ActionOps(p)
}

trait IndexSyntax {
  implicit def indexOps[T](t: T) = new IndexOps(t)
}

trait LexicoSyntax {
  implicit def LexicoOps[P: Lexico](lhs: P) = new LexicoOps(lhs)
}

trait LexicoSeqSyntax extends LexicoSyntax {
  implicit def LexicoSeqOps[P: LexicoSeq](lhs: P) = new LexicoSeqOps(lhs)
}

trait AllSyntax extends
    ActionSyntax with
    IndexSyntax with
    LexicoSyntax with
    LexicoSeqSyntax
