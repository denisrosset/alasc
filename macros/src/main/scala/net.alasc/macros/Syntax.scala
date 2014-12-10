package net.alasc.macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context}

object Ops extends machinist.Ops {
  val operatorNames: Map[String, String] =
    spire.macros.Ops.operatorNames ++ Map(
      // partial operations ?+? |+|? |-|? |+|! |-|!
      ("$qmark$plus$qmark", "isOpDefined"),
      ("$qmark$minus$qmark", "isOpInverseDefined"),
      ("$bar$plus$bar$qmark", "partialOp"),
      ("$bar$minus$bar$qmark", "partialOpInverse"),
      ("$bar$plus$bar$bang", "forceOp"),
      ("$bar$minus$bar$bang", "forceOpInverse"),

      // partial actions ?|+|> <|+|? !|+|> <|+|!
      ("$qmark$bar$plus$bar$greater", "partialActl"),
      ("$less$bar$plus$bar$qmark", "partialActr"),
      ("$qmark$bar$plus$bar$greater", "forceActl"),
      ("$less$bar$plus$bar$qmark", "forceActr")
    )
}
