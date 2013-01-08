package object perm {
  //package com.bxwrld.combinatorics

  // import com.bxwrld.combinatorics._

  import scala.collection.mutable
  import scala.collection.immutable

  type Domain = Int

  implicit def empowerMyDomain(alpha: Domain) = new EmpoweredDomain(alpha)
  implicit def Cycle2Permutation(value: Cycle) = value.toPermutation(value.size)
  
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
  class Permutation(P: Seq[Domain]) {

    /** Image of integers 0..n-1 under the permutation */
    val image = Vector[Domain](P:_*)

    // Private (mutable) variables are cached computations

    /** Cached decomposition over cycles, in the form of a sorted list of
      * pairs (minimal element of cycle, cycle length), sorted by minimal element
      * of cycle */
    private var _cycles: Option[List[(Domain, Int)]] = None
    import scala.annotation.tailrec

    def cycle(start: Domain): List[Domain] = {
      def walk(l: List[Domain], el: Domain): List[Domain] = {
        if (el == start)
          l
        else
          el :: walk(l, image(el))
      }
      walk(List(start), image(start))
    }

    // Standard scala methods
    override def toString = {
      if(Permutation.printCycles) {
        def cycleStr(i: (Domain, Int)): String = Cycle(cycle(i._1):_*).toStringWithoutName
        val cyclesStr = cycles(false).map(cycleStr(_)).mkString("","","")
        if (hasInSupport(size - 1))
          "Cycle" + cyclesStr
        else
          "Permutation(" + size + ")" + cyclesStr
      } else
        image.mkString("Permutation(",", ",")")
    }

    override def equals(other: Any): Boolean = other match {
      case that: Permutation => that.image.sameElements(this.image)
      case _ => false
    }

    override def hashCode = image.hashCode

    // Support for notation Permutation(5)(0,1)(2,3)
    def apply(elems: Domain*): Permutation = this * Cycle(elems:_*).toPermutation(size)

    // Simple properties
    def size = image.length

    // Permutation as group element
    def inverse: Permutation = {
      val a = Array.fill[Domain](size)(0)
      for ((e, i) <- image.view.zipWithIndex) a(e) = i
      Permutation(a:_*)
    }

    def *(other: Permutation) = {
      val a = image
      val b = other.image ++ (other.size until size)
      new Permutation((for(i <- a) yield b(i)) ++ b.slice(a.length, b.length))
    }

    // Mathematical properties
    def support = for(i <- 0 until size if hasInSupport(i)) yield i
    def hasInSupport(el: Domain) = (image(el) != el)
    def isIdentity: Boolean = !(0 until size).exists( hasInSupport(_) )

    /* Computes all the cycles of this permutation, returns a list
     * of (minimal el. of cycle, cycle length)
     * @param includeTrivialCycles if true, the result list contains also cycles
     *                             consisting of one element
     */
    def cycles(includeTrivialCycles: Boolean = false): List[(Domain, Int)] = {
      if (_cycles.isEmpty) {
        var checked = mutable.BitSet(size)
        var i = size - 1
        var cycleList = List.empty[(Domain, Int)]
        while (i >= 0) {
          if(!checked(i)) {
            var minEl = i
            var j = i
            var cycleLength = 0
            do {
              checked(j) = true
              if (minEl > j)
                minEl = j
              j = image(j)
              cycleLength += 1
            } while (j != i)
            if (cycleLength > 1)
              cycleList = (minEl, cycleLength) :: cycleList
          }
          i -= 1
        }
        import scala.math.Ordering.Implicits._
        _cycles = Some(cycleList.sortWith(_<_))
      }
      if (includeTrivialCycles)
        _cycles.get ++ support.map( ((_, 1)) )
      else
        _cycles.get
    }
  }


  object Permutation {
    def apply(args: Domain*): Permutation = {
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

  class EmpoweredDomain(alpha: Domain) {
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
    //def **(G: PermutationGroup) = G.orbit(alpha)
  }

  trait Orbit {
    def contains(el: Domain): Boolean
    def iterable: Iterable[Domain]
  }

  class OrbitSet(set: immutable.SortedSet[Domain]) extends Orbit {
    val orbit = set
    override def contains(el: Domain) = orbit.contains(el)
    override def iterable = orbit
  }

  object OrbitSet {
    def fromGenerators(alpha: Int, generators: Iterable[Permutation]): OrbitSet = {
      val Delta = mutable.HashSet.empty[Int]
      def visit(a: Int) {
        if (!Delta(a)) {
          Delta += a
          for (g <- generators) visit(a**g)
        }
      }
      visit(alpha)
      new OrbitSet(immutable.SortedSet.empty[Int] ++ Delta)
    }
  }

  trait Transversal extends Orbit {
    def apply(el: Int): Permutation
    def size: Int
  }

  class EmptyTransversal(alpha: Domain, degree: Int) extends Transversal {
    override def size = 1
    override def apply(el: Int) = {
      assert(el == alpha)
      Permutation(degree)
    }
    override def contains(el: Int) = (el == alpha)
    override def iterable = List(alpha)
  }

  class ExplicitTransversal(explicitMap: immutable.TreeMap[Int, Permutation]) extends Transversal {
    override def toString: String = (for ((key, value) <- explicitMap) yield key + " => " + value).mkString("","\n","")
    override def size = explicitMap.size
    override def apply(el: Int): Permutation = explicitMap(el)
    override def contains(el: Int): Boolean = explicitMap.contains(el)
    override def iterable = explicitMap.keys
  }

  object ExplicitTransversal {
    def fromGenerators(alpha: Int, generators: Iterable[Permutation]): ExplicitTransversal = {
      val m = mutable.HashMap.empty[Int, Permutation]
      val degree = generators.head.size
      def visit(a: Int, p: Permutation) {
        if (!m.isDefinedAt(a)) {
          m(a) = p.inverse
          for ((g, i) <- generators.view.zipWithIndex)
            visit(a**g, g.inverse*p)
        }
      }
      visit(alpha, Permutation(degree))
      new ExplicitTransversal(immutable.TreeMap.empty[Int, Permutation] ++ m)
    }
  }

  class PartialBSGS(B: List[Int], S: Set[Permutation], X: Seq[Permutation]) {
    def verify  {
      for(x <- X) assert(S.contains(x))
      for(s <- S) assert(S.contains(s.inverse))
      for(s <- S) assert(B.exists(s.hasInSupport(_)))
    }

  }

  object BaseConstruction {
    def chooseBaseElement(h: Permutation, base: Seq[Domain]): Option[Int] = {
      for (beta <- 0 until h.size if !base.contains(beta))
        if (h.hasInSupport(beta))
          return Some(beta)
      return None
    }

    def setup(prescribedBase: List[Domain], generators: List[Permutation]): (mutable.ArrayBuffer[Domain], mutable.ArrayBuffer[Transversal], mutable.ArrayBuffer[List[Permutation]]) = {
      val B = mutable.ArrayBuffer.empty[Domain] ++ prescribedBase
      val U = mutable.ArrayBuffer.empty[Transversal]
      val S = mutable.ArrayBuffer.empty[List[Permutation]]
      val n = generators.head.size
      val nonIdentityGenerators = generators.filter(!_.isIdentity)
      // extend base so that no group element fixes all base elements
      for (g <- nonIdentityGenerators) {
        if (!B.exists(beta => g.hasInSupport(beta))) {
          chooseBaseElement(g, B) match {
            case Some(beta) => B += beta
            case None => { }
          }
        }
      }
      if (B.isEmpty) {
        B += 0
        val trivialTransversal:Map[Int, Permutation] = Map( (0, Permutation(generators.head.size)) )
        U += new ExplicitTransversal(immutable.TreeMap.empty[Int, Permutation] ++ trivialTransversal)
        S += List.empty[Permutation]
        return (B, U, S)
      }
      for ((b,i) <- B.view.zipWithIndex) {
        val Si = nonIdentityGenerators.filter(g => !(0 until i).exists(j => g.hasInSupport(B(j))))
        U += ExplicitTransversal.fromGenerators(b, Si)
        S += Si
      }
      return (B, U, S)
    }
  }

  object SchreierSimsConstruction {
    def construct(prescribedBase: List[Domain], generators: List[Permutation]): BSGS = {
      val tuple = BaseConstruction.setup(prescribedBase, generators)
      val B = tuple._1
      val U = tuple._2
      val S = tuple._3
      val n = generators.head.size
      // we do not do the Schreier generator optimization
      var j = B.size
      while (j >= 1) {
        def iterateOverBaseAndGenerators: Boolean = {
          for(beta <- U(j-1).iterable; x <- S(j-1)) {
            val el = U(j-1)(beta)*x*(U(j-1)(beta**x).inverse)
            // sift for S_{j+1} so use index j here
            val (h, k) = BSGS.sift(el, B.drop(j), U.drop(j))
            if (k < B.size - j  || !h.isIdentity) {
              if (j == B.size) {
                BaseConstruction.chooseBaseElement(h, B) match {
                  case Some(gamma) => B += gamma
                  case None => { 
                    println(h)
                    println(B)
                    println(U)
                    println(S)
                    println( (k,j) )
                    assert(false) }
                }
                assert(j < B.size)
                S += List.empty[Permutation]
                U += new EmptyTransversal(B(U.size), n)
              }
              S(j) = h :: S(j)
              // TODO: more efficient U(j) = U(j).orbitUpdate(j, S(j), h)
              U(j) = ExplicitTransversal.fromGenerators(B(j), S(j))
              j += 1
              return false
            }
          }
          return true
        }
        if (iterateOverBaseAndGenerators)
          j -= 1
      }
      val SGS = mutable.HashSet.empty[Permutation]
      for (s <- S) SGS ++= s
      return new BSGS(B.toList, SGS.toList, U.toList, n)
    }
  }

  class BSGS(B: List[Domain], S: List[Permutation], U: List[Transversal], n: Int) {
    val base: List[Domain] = B
    val strongGeneratingSet: List[Permutation] = S
    val transversals: List[Transversal] = U
    val degree: Int = n

    def order: Int = (transversals :\ 1)((kv:Transversal, p: Int) => kv.size*p)

    def sifts(g: Permutation): Boolean = {
      val (siftee, m) = sift(g)
      m == base.size && siftee.isIdentity
    }

    def sift(g: Permutation, j: Int, k: Int): (Permutation, Int) = BSGS.sift(g, base.view(j, k), transversals.view(j, k))

    def sift(g: Permutation, j: Int = 0): (Permutation, Int) = BSGS.sift(g, base.drop(j), transversals.drop(j))
  }

    object BSGS {
      def sift(g: Permutation, base: Iterable[Int], transversals: Iterable[Transversal]): (Permutation, Int) = {
        var siftee = g
        var k = 0
        (base, transversals).zipped map ( (b, Ui) => {
          val beta = b**siftee
          if (!Ui.contains(beta))
            return (siftee, k)
          siftee = siftee * Ui(beta).inverse
          k += 1
        } )
        (siftee, k)
      }
      def siftEx(g: Permutation, base: Iterable[Int], transversals: Iterable[Transversal]): (Permutation, List[Permutation], Int) = {
        var siftee = g
        var k = 0
        var lprod = List.empty[Permutation]
        (base, transversals).zipped map ( (b, Ui) => {
          val beta = b**siftee
          if (!Ui.contains(beta))
            return (siftee, lprod, k)
          lprod = Ui(beta) :: lprod
          siftee = siftee * Ui(beta).inverse
          k += 1
        } )

        (siftee, lprod, k)
      }
    }
/*
import perm._
val a = Permutation(4)(0,1)
val b = Permutation(4)(1,2)
val c = Permutation(4)(2,3)
val d = Permutation(4)(3,0)
val i = Permutation(4)
val S = List(List(a,b,c,d), List(a,b), List(a), List(i))
val B = List(3,2,1)
val T = (B, S).zipped map ( (b, s) => ExplicitTransversal.fromGenerators(b, s) )
val g = Permutation(4)(0,3,1)
BSGS.siftEx(g, B, T)


 */
  class PermutationGroup(G: Seq[Permutation]) {
    val generators = immutable.TreeSet[Permutation](G:_*)

    /** Degree of the group, possibly not minimal! */
    val degree = G.head.size

    override def toString = generators.mkString("PermutationGroup(", ", ", ")")

    /** Verifies sane assumptions about this group. For now:
      * - all generators have the size of this group degree
      */
    def verify: Boolean = {
      for (g <- generators if g.size != degree) return false
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
/*    def schreierVector(alpha: Int): Array[Int] = {
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
    }*/
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
      *//*
    def transversal(alpha: Int): Map[Int, Permutation] = PermutationGroup.transversal(alpha, generators)
    
    def schreierSims(baseseq: Seq[Int]): (List[Int], List[Permutation]) = {
      assert(!generators.isEmpty) // this group is not the trivial group
      val base = mutable.ArrayBuffer.empty[Int] ++ baseseq
      if (base.isEmpty)
        base += (0 to degree).find(orbit(_).size > 1).get
      var stabs = PermutationGroup.distributeGeneratorsByBase(base, generators)
      var orbits = (base, stabs).zipped.map(PermutationGroup.orbit(_,_))
      var transversals = (base, stabs).zipped.map(PermutationGroup.transversal(_,_))
      var j: Int = 0
      while (j >= 0) {
        def iterateOrbitStab: Boolean = {
          for(beta <- orbits(j); x <- stabs(j)) {
            val ubeta = transversals(j)(beta)
            val ubetaxinv = transversals(j)(beta**x).inverse
            val (h, k) = PermutationGroup.sifting(base, orbits, transversals, ubeta*x*ubetaxinv, j + 1)
            if (k + j < base.length - 1 || !h.isIdentity) {
              if (j > base.length - 1) {
                // h fixes all the base points
                assert(!base.exists(b => b**h != b))
                val newbeta = (0 until degree).find(b => b**h != b).get
                stabs = stabs :+ List.empty[Permutation]
                orbits = orbits :+ SortedSet.empty[Int]
                transversals = transversals :+ Map.empty[Int, Permutation]
              }
              stabs(j) = h :: stabs(j)
              orbits(j) = PermutationGroup.orbit(base(j), stabs(j))
              transversals(j) = PermutationGroup.transversal(base(j), stabs(j))
              j += 1
              assert(j < stabs.length)
              assert(j < orbits.length)
              assert(j < transversals.length)
              return false
            }
          }
          return true
        }
        if (iterateOrbitStab)
          j -= 1
      }
      val S = mutable.HashSet.empty[Permutation]
      for(s <- stabs) S ++= s
      (base.toList, S.toList)
    }
         */
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
//    def orbit(alpha: Int) = PermutationGroup.orbit(alpha, generators)
  }

  object PermutationGroup {
    def apply(args: Permutation*): PermutationGroup = {
      val degree = args.map(_.size).max
      new PermutationGroup(args.filter(!_.isIdentity).map(i => if(i.size < degree) Permutation(degree)*i else i).toList)
    }
/*
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


    def sifting(base: Seq[Int], basicOrbits: Seq[SortedSet[Int]], transversals: Seq[Map[Int, Permutation]], g: Permutation, start: Int = 0): (Permutation, Int) = {
      var h = g
      for (i <- start until base.size) {
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
 */
  }
}


/*
import perm._
val a = Permutation(6)(0,1,4)
val b = Permutation(6)(0,3)(2,4)
val BSGS = SchreierSimsConstruction.construct(List.empty[Domain], List(a,b))

 */
