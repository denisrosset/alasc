package com.bxwrld.combinatorics

// import com.bxwrld.combinatorics._
object Cycle {
  def apply(elems: Int*): Permutation = {
    require(elems.size == elems.distinct.size)
    var a = (0 to elems.max).toArray
    val rot = elems.tail.toList ::: List(elems.head)
    for ((i,j) <- elems zip rot) a(i) = j
    new Permutation(a)
  }
}

class Permutation(P: Array[Int]) {
  val arrayForm: Array[Int] = P

  private var _cyclicForm = List[List[Int]]()

  override def toString = {
    if(Permutation.printCyclic) {
      val cycles = cyclicForm.map(_.mkString("(",", ",")")).mkString("","","")
      if(arrayForm(arrayForm.length - 1) == arrayForm.length - 1)
        "Permutation(" + arrayForm.length + ")" + cycles
      else
        "Cycle" + cycles
    } else
      arrayForm.mkString("Permutation(",", ",")")
  }

  override def equals(other: Any): Boolean = other match {
    case that: Permutation => that.arrayForm.sameElements(this.arrayForm)
    case _ => false
  }

  def apply(elems: Int*): Permutation = this * Cycle(elems:_*)

  def support = for((e, i) <- arrayForm.view.zipWithIndex if arrayForm(i) != i) yield i

  def invert: Permutation = {
    val a = Array.fill[Int](arrayForm.length)(0)
    for ((e, i) <- arrayForm.view.zipWithIndex) a(e) = i
    Permutation(a:_*)
  }

  def isIdentity: Boolean = !arrayForm.indices.exists(i => i != arrayForm(i))

  def *(other: Permutation) = {
    val a = arrayForm
    val b = other.arrayForm ++ (other.arrayForm.length until this.arrayForm.length)
    new Permutation((for(i <- a) yield b(i)) ++ b.slice(a.length, b.length))
  }
  /* This is used to convert to the cyclic notation from the canonical
   * notation. Singletons are omitted.
   * 
   * Examples
   * ========
   * 
   * scala> import com.bxwrld.combinatorics._
   * scala> Permutation.printCyclic = false
   * scala> val p = Permutation(0, 3, 1, 2)
   * scala> p.cyclicForm
   * res: List[List[Int]] = List(List(1, 3, 2))
   * scala> Permutation(1, 0, 2, 4, 3, 5).cyclicForm
   * reS: List[List[Int]] = List(List(0, 1), List(3, 4))
   */
  def cyclicForm: List[List[Int]] = {
    import scala.math.Ordering.Implicits._
    if (_cyclicForm.nonEmpty)
      return _cyclicForm
    var unchecked: Array[Boolean] = Array.fill(arrayForm.length)(true)
    var c_form = List[List[Int]]()
    for(i <- arrayForm.indices) {
      if(unchecked(i)) {
        var cycle = List[Int](i)
        unchecked(i) = false
        var j = i
        while(unchecked(arrayForm(j))) {
          j = arrayForm(j)
          cycle = j :: cycle
          unchecked(j) = false
        }
        if (cycle.length > 1) {
          c_form = cycle.reverse :: c_form
        }
      }
    }
    _cyclicForm = c_form.sortWith(_<_)
    _cyclicForm
  }
}

object Permutation {

  def apply(args: Int*): Permutation = {
    if(args.length == 1 && args(0) > 0)
      return new Permutation((0 until args(0)).toArray)
    new Permutation(args.toArray)
  }
  var printCyclic = false
}

class PermutationGroup(g: List[Permutation]) {
  val generators = g
  val r = g.length
  val degree = g(0).arrayForm.length
  var _order: Option[Int] = None
  var _center: Option[Int] = None // TODO not true type
  var _isAbelian: Option[Boolean] = None
  var _isTransitive: Option[Boolean] = None
  var _isSym: Option[Boolean] = None
  var _isAlt: Option[Boolean] = None
  var _isPrimitive: Option[Boolean] = None
  var _isNilpotent: Option[Boolean] = None
  var _isSolvable: Option[Boolean] = None
  var _isTrivial: Option[Boolean] = None
  var _transitivityDegree: Option[Int] = None
  var _maxDiv: Option[Int] = None
  // these attributes are assigned after running schreierSims - TODO wrong types
  var _base: Option[Int] = None
  var _strongGens: Option[Int] = None
  var _basicOrbits: Option[Int] = None
  var _transversals: Option[Int] = None
  // these attributes are assigned after running randomPrInit
  // TODO: add them

  /* Extend a sequence of points and generating set to a base and strong
   * generating set.
   * 
   * @param base The sequence of points to be extended to a base.
   *             Optional parameter with default value ``[]``.
   * @param gens The generating set to be extended to a strong generating 
   *             set relative to the base obtained. Optional parameter 
   *             with default value ``self.generators``.
   * 
   * @return (base, strong_gens)
   *        ``base`` is the base obtained, and ``strong_gens`` is the strong
   *          generating set relative to it. The original parameters ``base``,
   *        ``gens`` remain unchanged.
   * 
   * Examples
   * ========
   * >>> from sympy.combinatorics.named_groups import AlternatingGroup
   >>> from sympy.combinatorics.perm_groups import PermutationGroup
   >>> from sympy.combinatorics.testutil import _verify_bsgs
   >>> A = AlternatingGroup(7)
   >>> base = [2, 3]
   >>> seq = [2, 3]
   >>> base, strong_gens = A.schreier_sims_incremental(base=seq)
   >>> _verify_bsgs(A, base, strong_gens)
   True
   >>> base[:2]
   [2, 3]

   Notes
   =====

   This version of the Schreier-Sims algorithm runs in polynomial time.
   There are certain assumptions in the implementation - if the trivial
   group is provided, ``base`` and ``gens`` are returned immediately,
   as any sequence of points is a base for the trivial group. If the
   identity is present in the generators ``gens``, it is removed as
   it is a redundant generator.
   The implementation is described in [1], pp. 90-93.

   */
  def schreierSimsIncremental(baselist: List[Int] = List[Int](), genlist: List[Permutation] = generators): (List[Int], List[Permutation]) = {
    import scala.collection.immutable.TreeMap
    import scala.collection.mutable.ArrayBuffer

    // handle the trivial group
    if (genlist.length == 1 && genlist(0).isIdentity)
      return (baselist, genlist)
    // remove the identity as a generator
    val gens = genlist.filter(!_.isIdentity)
    var base = baselist
    // make sure no generator fixes all base points
    for(g <- gens) {
      if (!base.exists(i => i != g.arrayForm(i))) {
        val firstNotFixed = (0 until degree).find(i => i != g.arrayForm(i)).get
        base = firstNotFixed :: base
      }
    }
    // distribute generators according to basic stabilizers
    var strongGensDistr = PermutationGroup.distributeGensByBase(base, gens).toArray
    // initialize the basic stabilizers, basic orbits, and basic transversals
    var transversals = Array.tabulate(base.length) { i =>
      PermutationGroup.orbitTransversal(degree, strongGensDistr(i), base(i)).toMap }
    // orbs(i) is just transversals(i).keys
    var i = base.length - 1
    while(i >= 0) {
      // this flag is used to continue with the main loop from inside
      // a nested loop
      var continuei = false
      import scala.util.control.Breaks._
      breakable {
        // test the generators for being a strong generating set
        var db = TreeMap.empty[Int, Permutation]
        for ((beta, ubeta) <- transversals(i)) {
          for (gen <- strongGensDistr(i)) {
            val gb = gen.arrayForm(beta)
            val t = transversals(i)
            val u1 = t(gb)
            val g1 = ubeta * gen
            if (g1 != u1) {
              // test if the schreier generator is in the i+1-th
              // would-be basic stabilizer
              var y = true
              val u1Inv: Permutation = db.get(gb) match {
                case Some(el) => el
                case None => {
                  val in = u1.invert
                  db = db + ((gb, in))
                  in
                }
              }
              val schreierGen = g1 * u1Inv
              val (h, j) = PermutationGroup.optimizedStrip(schreierGen,
                base, transversals.map(_.keys.toList), transversals, i)
              if (j <= base.length) {
                // new strong generator h at level j
                y = false
              } else if (h.isDefined) {
                y = false
                var moved = 0
                while (h.get.arrayForm(moved) == moved)
                  moved += 1
                base = base :+ moved
                strongGensDistr = strongGensDistr :+ List.empty[Permutation]
              }
              if (!y) {
                transversals = transversals.padTo(j, TreeMap.empty[Int, Permutation])
                for(l <- i+1 until j) {
                  strongGensDistr(l) = strongGensDistr(l) :+ h.get
                  transversals(l) = PermutationGroup.orbitTransversal(degree, strongGensDistr(i), base(i)).toMap
                }
                i = j - 1
                continuei = true
              }
            }
            if (continuei)
              break
          }
        }
      }
      if (!continuei)
        i -= 1
    }
    var strongGens = List.empty[Permutation]
    for (gens1 <- strongGensDistr) {
      for (gen <- gens1) {
        if(!strongGens.exists(_ == gen))
          strongGens = strongGens :+ gen
      }
    }
    (base, strongGens)
  }
}

object PermutationGroup {
  /** Distribute the group elements ``gens`` by membership in basic stabilizers.
    * 
    * Notice that for a base `(b_1, b_2, ..., b_k)`, the basic stabilizers
    *     are defined as `G^{(i)} = G_{b_1, ..., b_{i-1}}` for
    * `i \in\{1, 2, ..., k\}`.
    * 
    * @param base a sequence of points in `\{0, 1, ..., n-1\}`
    * @param gens a list of elements of a permutation group of degree `n`.
    * 
    * @return a list of length `k`, where `k` is the length of ``base``. 
    *        The `i`-th entry contains those elements in ``gens`` which fix the
    *        first `i` elements of ``base`` (so that the `0`-th entry is equal to
    *        ``gens`` itself). If no element fixes the first `i` elements of ``base``,
    *        the `i`-th element is set to a list containing the identity element.
    * 
    * Examples
    * ========
    * scala> import com.bxwrld.combinatorics._
    * scala> val base = List(0,1)
    * scala> val strong_gens = List(Permutation(3)(0,1,2), Permutation(3)(0,2), Permutation(3)(1,2))
    * scala> Permutation.printCyclic = true
    * scala> PermutationGroup.distributeGensByBase(base, gens)
    * res: List[List[com.bxwrld.combinatorics.Permutation]] = List(List(Cycle(0, 1, 2), Cycle(0, 2), Cycle(1, 2)), List(Cycle(1, 2)))
    */
  def distributeGensByBase(base: List[Int], gens: List[Permutation]): List[List[Permutation]] = {
    import scala.collection.mutable.ListBuffer
    val degree = gens(0).arrayForm.length
    var stabs = ListBuffer(base.indices.map(x => ListBuffer[Permutation]()):_*)
    var max_stab_index = 0
    for(gen <- gens) {
      val j: Int = base.indices.find(j => gen.arrayForm(base(j)) != base(j)).get
      max_stab_index = if(j > max_stab_index) j else max_stab_index
      for(k <- 0 to j)
        stabs(k) += gen
    }
    for(i <- max_stab_index + 1 until base.length)
      stabs(i) += Permutation(degree)
    stabs.map(_.toList).toList
  }

  /** Computes a transversal for the orbit of ``alpha`` as a set.
    * 
    * @param generators   generators of the group ``G``
    * 
    * For a permutation group ``G``, a transversal for the orbit
    * ``Orb = \{g(\alpha) | g \in G\}`` is a set
    * ``\{g_\beta | g_\beta(\alpha) = \beta\}`` for ``\beta \in Orb``.
    * Note that there may be more than one possible transversal.
    * It returns the list of pairs ``(\beta, g_\beta)``.
    * For a proof of correctness, see [1], p.79
    * 
    * Examples
    * ========
    * 
    * scala> import com.bxwrld.combinatorics._
    * scala> Permutation.printCyclic = true
    * scala> val degree = 6
    * scala> val generators = List(Permutation(6)(0, 1, 2, 3, 4, 5), Permutation(6)(0, 5)(1, 4)(2, 3))
    * scala> PermutationGroup.orbitTransversal(degree, generators, 0)
    * res:  List[(Int, com.bxwrld.combinatorics.Permutation)] = List((0,Permutation(6)), (1,Cycle(0, 1, 2, 3, 4, 5)), (5,Cycle(0, 5)(1, 4)(2, 3)), (2,Cycle(0, 2, 4)(1, 3, 5)), (4,Permutation(6)(0, 4)(1, 3)), (3,Cycle(0, 3)(1, 4)(2, 5)))
    */
  def orbitTransversal(degree: Int,
    generators: List[Permutation],
    alpha: Int): List[(Int, Permutation)] = {
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.ArrayBuffer
    var tr = ArrayBuffer[(Int, Permutation)]((alpha, Permutation(degree)))
    var used = ArrayBuffer.tabulate(degree) { _ == alpha }
    var i = 0
    do {
      val (x, px) = tr(i)
      generators.foreach (gen => {
        val temp = gen.arrayForm(x)
        if(!used(temp)) {
          tr += ((temp, px * gen))
          used(temp) = true
        }
      })
      i += 1
    } while (i < tr.length)
      tr.toList
  }

  def strip(g: Permutation,
    base: List[Int],
    orbits: List[List[Int]],
    transversals: List[Map[Int, Permutation]]): (Permutation, Int) = {
    var h = g
    for((b, i) <- base.view.zipWithIndex) {
      val beta = h.arrayForm(b)
      if(beta != b) {
        if(!orbits(i).exists(_ == beta))
          return ((h, i + 1))
        h = h * (transversals(i)(beta).invert)
      }
    }
    return ((h, base.length + 1))
  }

  def optimizedStrip(g: Permutation,
    base: Seq[Int],
    orbits: Seq[Seq[Int]],
    transversals: Seq[Map[Int, Permutation]],
    j: Int): (Option[Permutation], Int) = {
    var h = g
    for(i <- j+1 until base.length) {
      val b = base(i)
      val beta = h.arrayForm(b)
      if (beta != b) {
        if(!orbits(i).exists(_ == beta))
          return ((Some(h), i + 1))
        val u = transversals(i)(beta)
        if (h == u)
          return ((None, base.length + 1))
        h = h * u.invert
      }
    }
    return ((Some(h), base.length + 1))
  }
}
