package net.alasc

import bsgs._
import scala.math.{min, max}
import scala.util.Random
import scala.reflect.ClassTag

/** All-around group class. Constructs a BSGS behind the scenes as needed.
  * 
  * The group can be constructed by using the following algorithms (listed
  * below by decreasing priority):
  * 
  * - the random Schreier-Sims algorithm if all the following elements are provided:
  *   - the order of the group,
  *   - a random number generator (by default, [[scala.util.Random]] will be used),
  *   - a way to generate random group elements, using either:
  *     - a random element function,
  *     - creating a [[RandomBag] from the list of generators.
  * - the deterministic Schreier-Sims algorihm if the group generators are known.
  */
class Group[F <: FiniteElement[F] : ClassTag](
  val faithfulAction: Action[F, Perm], 
  val identity: F,
  protected var knownGenerators: Option[List[F]] = None,
  protected var knownBaseForStrongGeneratorsAndAction: Option[Base] = None, // TODO: allow other base than the FullBase
  protected var knownOrder: Option[BigInt] = None,
  protected var knownRandom: Option[Random => F] = None,
  protected var knownRandomIsUniformStateless: Boolean = false,
  protected var randomGenerator: Option[Random] = Some(scala.util.Random),
  protected var knownBSGS: Option[BSGSGroup[ActionElement[F, Perm]]] = None,
  protected var transBuilder: TransBuilderLike = ExpTransBuilder,
  protected var baseStrategy: BaseStrategy = FullBase
) extends FiniteGroup[F] {
  if (knownBSGS.isDefined) {
    order
    generators
  }

  def throwIncomplete = throw new IllegalArgumentException("Group information is incomplete. Lookup the Group class documentation for possible combinations.")

  def actionElement(f: F) = ActionElement(f, faithfulAction)

  def randomFromGeneratorsOnly = knownRandom match {
    case Some(rfun) => rfun
    case None => {
      require_(randomGenerator.isDefined)
      val gens = knownGenerators.getOrElse(throwIncomplete)
      val bag = RandomBag(gens, identity, min(10, gens.length), 50, randomGenerator.get)
      val rfun = (gen: Random) => bag.randomElement(gen)
      knownRandom = Some(rfun)
      knownRandomIsUniformStateless = false
      rfun
    }
  }

  def bsgs = knownBSGS match {
    case Some(b) => b
    case None => {
      val bsgs = (knownBaseForStrongGeneratorsAndAction, knownGenerators) match {
        // if the user provides a base, this means the provided generators from a strong generating set
        case (Some(base), Some(sgs)) => BSGS.fromBaseAndStrongGeneratingSet(base, sgs.map(actionElement(_)), actionElement(identity), transBuilder)
        // if not, we have to use the Schreier Sims construction
        case _ => (randomGenerator, knownOrder, knownRandom, knownGenerators) match {
          // either the randomized Schreier-Sims algorithm with the user provided random function
          case (Some(rg), Some(ko), Some(r), _) => BSGS.randomSchreierSims(actionElement _ compose r, ko, actionElement(identity), baseStrategy, transBuilder)(rg)
          // or the randomized Schreier-Sims algorithm with a random bag we construct out of the group generators
          case (Some(rg), Some(ko), _, Some(g)) => BSGS.randomSchreierSims(actionElement _ compose randomFromGeneratorsOnly, ko, actionElement(identity), baseStrategy, transBuilder)(rg)
          // as a fail-over, we use the deterministic Schreier-Sims algorithm
          case (_, _, _, Some(g)) => BSGS.schreierSims(g.map(actionElement _), actionElement(identity), baseStrategy, transBuilder)
          case _ => throwIncomplete
        }
      }
      knownBSGS = Some(bsgs)
      bsgs
    }
  }

  def generatorsList: List[F] = knownGenerators match {
    case Some(list) => list
    case None => {
      val g = bsgs.strongGeneratingSet.map(_.source)
      knownGenerators = Some(g)
      g
    }
  }

  def generators = generatorsList.iterator

  def randomElement(gen: Random) = (knownRandom, knownRandomIsUniformStateless) match {
    case (Some(rfun), true) => rfun(gen)
    case _ => {
      val rfun = (gen: Random) => bsgs.randomElement(gen).represents.source
      knownRandom = Some(rfun)
      rfun(gen)
    }
  }

  def order = knownOrder match {
    case Some(o) => o
    case None => {
      val o = bsgs.order
      knownOrder = Some(o)
      o
    }
  }

  def madeCompatible(that: Group[F]) = {
    assert(that.faithfulAction == faithfulAction)
    assert(bsgs.base == that.bsgs.base)
    that
  }

  def intersection(that: Group[F]) = {
    val newBSGS = bsgs.intersection(madeCompatible(that).bsgs)
    new Group(faithfulAction, 
      identity,
      knownBSGS = Some(newBSGS),
      randomGenerator = if (randomGenerator == that.randomGenerator) randomGenerator else None)
  }

  def &(that: Group[F]) = intersection(that)


  override def toString = (knownGenerators match {
    case Some(s) => "Group " + s.mkString("<", ", ", ">")
    case None => "Unconstructed group"
  }) + (knownOrder match {
    case Some(o) => " of order " + o
    case None => ""
  })

  def elements = bsgs.elements.map(_.represents.source)
  def compatible(f: F) = true
  def contains(f: F) = bsgs.contains(actionElement(f))
}

object Group {
  def apply[F <: FiniteElement[F] : ClassTag](faithfulAction: Action[F, Perm], gens: Iterable[F], identity: F)(implicit gen: Random) = {
    require_(!gens.isEmpty) // TODO: relax and allow trivial group
    val id = if (identity eq null) gens.head*gens.head.inverse else identity
    val g = new Group(faithfulAction, id, knownGenerators = Some(gens.toList))
    g
  }
  def apply[P <: PermElement[P] : ClassTag](gens: Iterable[P], identity: P) = {
    require_(!gens.isEmpty) // TODO: relax and allow trivial group
    val id = if (identity eq null) gens.head*gens.head.inverse else identity
    val g = new Group(PermConversionAction, id, knownGenerators = Some(gens.toList))
    g
  }
  def apply[F <: FiniteElement[F] : ClassTag](faithfulAction: Action[F, Perm], rand: Random => F, order: BigInt, identity: F)(implicit gen: Random) = {
    val id = if (identity eq null) { val e = rand(gen); e*e.inverse } else identity
    val g = new Group(faithfulAction, id, knownRandom = Some(rand), knownOrder = Some(order))
    g
  }
  def apply[P <: PermElement[P] : ClassTag](rand: Random => P, order: BigInt, identity: P)(implicit gen: Random) = {
    val id = if (identity eq null) { val e = rand(gen); e*e.inverse } else identity
    val g = new Group(PermConversionAction, id, knownRandom = Some(rand), knownOrder = Some(order))
    g
  }
}
