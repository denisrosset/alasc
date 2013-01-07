
//package com.bxwrld.combinatorics

// import com.bxwrld.combinatorics._

import scala.collection._

/** A disjoint cycle. */
class Cycle(C: immutable.Iterable[Int]) {
  /** Image of integers 0...n-1 under the cycle */
  val image = immutable.TreeMap[Int, Int](Cycle.wrappedPairs[Int](C):_*)

  /** String representation of Cycle */
  override def toString = toList.mkString("Cycle(", ", ", ")")

  def toStringWithoutName = toList.mkString("(", ", ", ")")

  /** Hashcode for Cycle */
  override def hashCode = image.hashCode

  def size = image.keys.max + 1

  /** Support for notation Cycle(0, 1, 2)(4, 5) */
  def apply(elems: Int*): Permutation = this*Cycle(elems:_*)

  /** Product of cycles is a permutation */
  def *(other: Permutation): Permutation = this.toPermutation(List(size, other.size).max)*other

  def *(other: Cycle): Permutation = {
    val psize = List(size, other.size).max
    this.toPermutation(psize) * other.toPermutation(psize)
  }

  def toList: List[Int] = {
    var l = List.empty[Int]
    val h = image.head._1
    var i = h
    do {
      l = i :: l
      i = image(i)
    } while (i != h)
      l.reverse
  }

  /** Verify this is a disjoint cycle */
  def verify: Boolean = {
    val h = image.head._1
    var i = h
    var count = 0
    do image.get(i) match {
      case None => return false
      case Some(e) => {
        i = e
        count += 1
      }
    } while (i != h)
      return count == image.size
  }

  def toPermutation(size: Int) = {
    var a = Array.tabulate[Int](size) { i => image.get(i) match {
      case None => i
      case Some(e) => e
    }
    }
    new Permutation(a)
  }
}

/** Factory to create a permutation using cycle notation */
object Cycle {
  /** Creates a cycle with given list of indices. */
  def apply(elems: Int*): Cycle = new Cycle(elems.toList)

  implicit def cycleOrdering: Ordering[Cycle] = {
    import scala.math.Ordering.Implicits._
    Ordering.fromLessThan(_.toList < _.toList)
  }

  /** Zip an iterable C with a copy of itself rotated to the left once.
    * 
    * Example
    * =======
    * 
    * scala> Cycle.wrappedPairs(List(1,2,3))
    * res: List[(Int, Int)] = List((1,2), (2,3), (3,1))
    */
  def wrappedPairs[T](C: immutable.Iterable[T]): List[(T,T)] = {
    val start:(List[(T,T)], T)  = (List.empty[(T,T)], C.head)
    def itfun[T](el:T, prev:(List[(T,T)], T)): (List[(T,T)], T) =
      ((((el, prev._2)) :: prev._1, el))
    C.foldRight(start)((a,b) => itfun[T](a,b))._1
  }

}

/** A permutation acting on integers 0...n-1. Is immutable.
  * 
  * Can be constructed in different ways:
  * - from a sequence P representing a permutation of the integers 0...n-1
  *   using new Permutation(P)
  * 
  *   Example:
  * 
  *   scala> val a = new Permutation(List(1,2,0,3,5,4))
  * 
  * - using the notation Permutation(n) to represent the identity on 0...n-1
  * - using the notation Permutation(n)(cycle_1)...(cycle_m) where cycle_i is
  *   a cycle on the integers 0...n-1
  * 
  *   Examples:
  *
  *   scala> val b = Permutation(6)(0,1,2)(4,5)
  *   scala> a == b
  *   res: Boolean = True
  * 
  * - using a product of cycles
  */ 
class Permutation(P: Seq[Int]) {

  /** Image of integers 0..n-1 under the permutation */
  val image = Vector[Int](P:_*)

  /** Cached decomposition over cycles */
  private var _cyclicForm: Option[List[Cycle]] = None

  // Standard scala methods
  override def toString = {
    if(Permutation.printCycles) {
      val cycles = cyclicForm.map(_.toStringWithoutName).mkString("","","")
      if (isInSupport(size - 1))
        "Cycle" + cycles
      else
        "Permutation(" + size + ")" + cycles
    } else
      image.mkString("Permutation(",", ",")")
  }

  override def equals(other: Any): Boolean = other match {
    case that: Permutation => that.image.sameElements(this.image)
    case _ => false
  }

  override def hashCode = image.hashCode

  // Support for notation Permutation(5)(0,1)(2,3)
  def apply(elems: Int*): Permutation = this * Cycle(elems:_*).toPermutation(size)

  // Simple properties
  def size = image.length

  // Permutation as group element
  def inverse: Permutation = {
    val a = Array.fill[Int](size)(0)
    for ((e, i) <- image.view.zipWithIndex) a(e) = i
    Permutation(a:_*)
  }

  def *(other: Permutation) = {
    val a = image
    val b = other.image ++ (other.size until size)
    new Permutation((for(i <- a) yield b(i)) ++ b.slice(a.length, b.length))
  }

  // Mathematical properties
  def support = for(i <- 0 until size if isInSupport(i)) yield i
  def isInSupport(e: Int) = (image(e) != e)
  def isIdentity: Boolean = support.length == 0

  /* This is used to convert to the cyclic notation from the canonical
   * notation. Singletons are omitted.
   * 
   * Examples
   * ========
   * 
   * scala> val p = Permutation(0, 3, 1, 2)
   * scala> p.cyclicForm
   * res: List[Cycle] = List(Cycle(1, 3, 2))
   * scala> Permutation(1, 0, 2, 4, 3, 5).cyclicForm
   * res: List[Cycle] = List(Cycle(0, 1), Cycle(3, 4))
   */
  def cyclicForm: List[Cycle] = {
    _cyclicForm match {
      case Some(c) => c
      case None => {
        var checked = mutable.BitSet(size)
        var i = 0
        var cyclicForm1 = List.empty[Cycle]
        do {
          if(!checked(i)) {
            var cycle = List.empty[Int]
            var j = i
            do {
              checked(j) = true
              cycle = j :: cycle
              j = image(j)
            } while (j != i)
              if (cycle.length > 1)
                cyclicForm1 = Cycle(cycle.reverse:_*) :: cyclicForm1
          }
          i += 1
        } while (i < size)
          import scala.math.Ordering.Implicits._
        cyclicForm1 = cyclicForm1.sortWith(_<_)
        _cyclicForm = Some(cyclicForm1)
        cyclicForm1
      }
    }
  }
}

object Permutation {
  def apply(args: Int*): Permutation = {
    if(args.length == 1 && args(0) > 0)
      return new Permutation((0 until args(0)).toArray)
    new Permutation(args.toList)
  }
  var printCycles = true

  implicit def permutationOrdering: Ordering[Permutation] = {
    import scala.math.Ordering.Implicits._
    Ordering.fromLessThan(_.image < _.image)
  }
}

class EmpoweredInt(alpha: Int) {
  /** Permutation acting on an element. Is left-associative as required.
    * 
    * Example
    * =======
    * 
    * scala> import PermImplicits._
    * scala> val a = Cycle(0,3,4)(1,2,5)
    * scala> val b = Cycle(1,2,0,5)
    * scala> 0**a
    * res: Int = 3
    * scala> 0**b**a
    * res: Int = 1
    * scala> 0**b
    * res: Int = 5
    * scala> 5**a
    * res: Int = 1
    * 
    */
  def **(P: Permutation) = P.image(alpha)

  /** Notation for the orbit of an element. */
  def **(G: PermutationGroup) = G.orbit(alpha)
}

object PermImplicits {
  implicit def empowerMyInt(alpha: Int) = new EmpoweredInt(alpha)
  implicit def Cycle2Permutation(value: Cycle) = value.toPermutation(value.size)
}

class PermutationGroup(G: Seq[Permutation]) {
  val generators = immutable.TreeSet[Permutation](G:_*)

  /** Degree of the group, possibly not minimal! */
  val degree = G.head.size

  override def toString = generators.mkString("PermutationGroup(", ", ", ")")

  /** Verifies sane assumptions about this group. For now:
    * - all generators have the size of this group degree
    * - the identity is not part of the generators
    */
  def verify: Boolean = {
    for (g <- generators if g.size != degree) return false
    for (g <- generators if g.isIdentity) return false
    true
  }

  /** Checks that @param candidate is a base for this group.
    * 
    * scala> import PermImplicits._
    * scala> val g = PermutationGroup(Cycle(0,1),Cycle(1,2),Cycle(2,3),Cycle(3,0))
    * scala> g.isBase(List(3,2))
    * res: Boolean = false
    * scala> g.isBase(List(3,2,1))
    * res: Boolean = true
    */
  def isBase(candidate: Seq[Int]): Boolean = {
    for (g <- generators) {
      if(!candidate.exists(i => i != g.image(i)))
        return false
    }
    return true
  }

  /** Returns a Schreier tree for alpha encoded as a vector, where the i-th cell
    * contains
    * - j >= 0 if the pair (i, i**(g(j)^-1)) is an edge of the Schreier tree
    * - -1 if i = alpha
    * - -2 if i is not in the orbit of alpha
    * 
    * Example
    * =======
    * 
    * scala> import PermImplicits._
    * scala> val a = Cycle(0,1,4)
    * scala> val b = Cycle(0,3)(2,4)
    * scala> val G = PermutationGroup(a,b)
    * scala> G.schreierVector(0)
    * res: Array[Int] = Array(-1, 0, 1, 1, 0)
    * scala> val c = Cycle(5,6)
    * scala> val G1 = Permutation(a,b,c)
    * scala> G1.schreierVector(0)
    * res: Array[Int] = Array(-1, 1, 2, 2, 1, -2, -2)
    */
  def schreierVector(alpha: Int): Array[Int] = {
    import PermImplicits._
    val v = Array.fill[Int](degree)(-2)
    val Delta = mutable.HashSet.empty[Int]
    def visit(a: Int, j: Int) {
      if (!Delta(a)) {
        Delta += a
        v(a) = j
        for ((g,i) <- generators.view.zipWithIndex)
          visit(a**g, i)
      }
    }
    visit(alpha, -1)
    v
  }
  /** Returns a transversal map for the element alpha
    * 
    * If i is in the orbit of alpha, then the map contains
    *  - i -> the permutation p such that i**p = alpha
    *  - i -> the identity if i = alpha
    * Points not in the orbit of alpha are not contained
    * 
    * Example
    * =======
    * 
    * scala> import PermImplicits._
    * scala> val a = Cycle(0,1,4)
    * scala> val b = Cycle(0,3)(2,4)
    * scala> val G = PermutationGroup(a,b)
    * scala> G.schreierVector(0)
    * res: Array[Int] = Array(-1, 0, 1, 1, 0)
    * scala> val c = Cycle(5,6)
    * scala> val G1 = Permutation(a,b,c)
    * scala> G1.transversal(0)
    * res: scala.collection.Map[Int,Permutation] = Map(0 -> Permutation(5), 1 -> Cycle(0, 4, 1), 2 -> Cycle(0, 3, 1, 4, 2), 3 -> Cycle(0, 3)(2, 4), 4 -> Cycle(0, 1, 4))
    */
  def transversal(alpha: Int): Map[Int, Permutation] = PermutationGroup.transversal(alpha, generators)
 
  def schreierSims(baseseq: Seq[Int]): (List[Int], List[Permutation]) = {
    assert(!generators.isEmpty) // this group is not the trivial group
    val base = mutable.ArrayBuffer.empty[Int] ++ baseseq
    if (base.isEmpty)
      base += (0 to degree).find(orbit(_).size > 1).get
    val stabs = PermutationGroup.distributeGeneratorsByBase(base, generators)
    val orbits = (base, stabs).zipped.map(PermutationGroup.orbit(_,_))
    val transversals = (base, stabs).zipped.map(PermutationGroup.transversal(_,_))
    var j = 0
    while (j >= 0) {
      def iterateOrbitStab: Boolean {
        for(beta <- orbits(j); x <- stabs(j)) {
          val (h, k) = PermutationGroup.sifting(base, orbits, transversals, )

        }
      }

    }

  }

  /** Orbit of a point @param alpha.
    * 
    * Example
    * =======
    * 
    * scala> import PermImplicits._
    * scala> val a = Cycle(0,3,4)(1,2,5)
    * scala> val b = Cycle(1,2,0,5)
    * scala> val g = PermutationGroup(a,b)
    * scala> g.orbit(0)
    * res: List[Int] = List(0, 1, 2, 3, 4, 5)
    */
  def orbit(alpha: Int) = PermutationGroup.orbit(alpha, generators)
}

object PermutationGroup {
  def apply(args: Permutation*): PermutationGroup = {
    val degree = args.map(_.size).max
    new PermutationGroup(args.filter(!_.isIdentity).map(i => if(i.size < degree) Permutation(degree)*i else i).toList)
  }

  def distributeGeneratorsByBase(base: Seq[Int], generators: Iterable[Permutation]): Array[List[Permutation]] = {
    val stabs = Array.fill(base.length)(List.empty[Permutation])
    val degree = generators.head.size
    var maxStabIndex = 0
    for (g <- generators) {
      val j = (0 until base.length - 1).find(i => g.image(base(i)) != base(i)) match {
        case None => base.length - 1
        case Some(e) => e
      }
      if (j > maxStabIndex)
        maxStabIndex = j
      (0 to j).foreach { i => stabs(i) = g :: stabs(i) }
    }
    stabs
  }

  def transversal(alpha: Int, generators: Iterable[Permutation]): Map[Int, Permutation] = {
    import PermImplicits._
    val m = mutable.HashMap.empty[Int, Permutation]
    val degree = generators.head.size
    def visit(a: Int, p: Permutation) {
      if (!m.isDefinedAt(a)) {
        m(a) = p
        for ((g, i) <- generators.view.zipWithIndex)
          visit(a**g, g.inverse*p)
      }
    }
    visit(alpha, Permutation(degree))
    immutable.TreeMap.empty[Int, Permutation] ++ m
  }

  def sifting(base: Seq[Int], basicOrbits: Seq[immutable.SortedSet[Int]], transversals: Seq[Map[Int, Permutation]], g: Permutation): (Permutation, Int) = {
    import PermImplicits._
    var h = g
    for (i <- base.indices) {
      val b = base(i)
      val basicOrbit = basicOrbits(i)
      val U = transversals(i)
      val beta = b**h
      if(!basicOrbit(beta))
        return (h, i - 1)
      val u = U.find(b**_._2 == beta).get._2
      h = h * u.inverse
    }
    return (h, base.length)
  }

  def orbit(alpha: Int, generators: Iterable[Permutation]): SortedSet[Int] = {
    import PermImplicits._
    val Delta = mutable.HashSet.empty[Int]
    def visit(a: Int) {
      if (!Delta(a)) {
        Delta += a
        for (g <- generators) visit(a**g)
      }
    }
    visit(alpha)
    immutable.SortedSet.empty[Int] ++ Delta
  }
}
