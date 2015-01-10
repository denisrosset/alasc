package net.alasc.laws

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Gen, Arbitrary, Prop}
import org.scalatest.FunSuiteLike
import org.typelevel.discipline.Laws

/** Discipline trait used to test laws on path dependent types
  * (example: a partition is an inner class of a domain 0...n-1).
  */
trait NestedDiscipline extends Discipline with FunSuiteLike {
  /** Checks the laws of the inner types of a outer instance of type `V`,
    * which has an arbitrary implicit generator.
    * 
    * @param name    Names of the outer and inner types
    * @param default Instance of the outer type, used to enumerate properties
    *                and type inference
    * @param f       Rule set builder for a inner type given an instance of `V`
    */
  def nestedCheckAll[V: Arbitrary](name: String, default: V)(f: V => Laws#RuleSet): Unit = {
    val defaultRuleSet = f(default)
    for (i <- defaultRuleSet.all.properties.indices) {
      test(name + "." + defaultRuleSet.all.properties(i)._1) {
        check(Prop.forAll( (v: V) => f(v).all.properties(i)._2 ))
      }
    }
  }
}
