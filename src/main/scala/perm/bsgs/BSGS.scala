package com.faacets.perm

package bsgs {
  abstract class BSGS[G <: PermutationGroup](val g: G) extends AbstractTransversalMixin {
    type UnderlyingGroup = G
    import scala.collection.mutable.ArrayBuffer
    def fromBaseAndSet(sgs: Seq[UnderlyingElement], base: Seq[Domain]) = {
      val sarr = (0 to base.length).toVector.map( i =>
        sgs.filter( s => base.take(i).forall( e => s.image(e) == e ) ) )
      val uarr = (0 until base.length).toVector.map( i => sarr(i).foldLeft(makeEmpty(base(i)))( _.addingGenerator(_) ) )
      BSGSGroup(sarr, uarr)
    }

/*
    def completeBase[P <: Permutation[P]](base: Vector[Domain], X: Seq[P]) = {
      var compBase = base
      var isComplete = true
      assert(X.forall(!_.isIdentity))
      do {
        isComplete = true
        X.find(x => compBase.forall( b => x.image(b) == b )) match {
          case Some(x) => {
            compBase = compBase :+ (0 until x.domainSize).find( k => x.image(k) != k ).get
            isComplete = false
          }
          case None => { }
        }
      } while (!isComplete)
        compBase
    }

    def setup[P <: Permutation[P], T <: Transversal[P, T]]
      (id: P, candBase: Vector[Domain], X: Seq[P])(implicit emptyU: (Domain, P) => T) = {
      val base = completeBase(candBase, X)
      // We compute the candidate generating sets for the stabilizer chain
      val S = ArrayBuffer((0 to base.length).map( i => X.filter( x => base.take(i).forall( e => x.image(e) == e ) ).toList ):_*)
      val U = ArrayBuffer((0 until base.length).map( i => S(i).foldLeft(emptyU(base(i), id))( _+_ ) ):_*)
      new BSGSConstruction[P, T](id, S, U)
    }

 */
    def constructionFullBase(xseq: Iterable[UnderlyingElement]) =  {
      val base = (0 until g.degree).toVector
      // We compute the candidate generating sets for the stabilizer chain
      val sarr = ArrayBuffer((0 to base.length).map( i => xseq.filter( x => base.take(i).forall( e => x.image(e) == e ) ).toList ):_*)
      val uarr = ArrayBuffer((0 until base.length).map( i => sarr(i).foldLeft(makeEmpty(base(i)))( _.addingGenerator(_) ) ):_*)
      BSGSConstruction(sarr, uarr)
    }

    def randomSchreierSims(cons: BSGSConstruction) = {
      while (cons.order < g.order)
        cons += g.randomElement
      BSGSGroup(cons.sarr, cons.uarr.toVector)
    }

    case class BSGSConstruction(
      sarr: ArrayBuffer[List[UnderlyingElement]],
      uarr: ArrayBuffer[Transversal]) extends BSGSBasics {
      type Group = BSGSConstruction
      type Element = BSGSConstructionElement
      def make(bvec: Vector[Domain]) = BSGSConstructionElement(bvec)
      case class BSGSConstructionElement(bvec: Vector[Domain]) extends BSGSElement {
        self: Element =>
        val group = BSGSConstruction.this
      }
      def +=(el: UnderlyingElement) = {
        var newGenerator = false
        val (_, h, j) = sift(el)
        if (j < length) // new strong generator at level j
          newGenerator = true
        else if (!h.isIdentity) { // new strong generator h fixes all the base points
          newGenerator = true
          val b = (0 until degree).find( h.hasInSupport(_) ).get
          sarr += List.empty[UnderlyingElement]
          uarr += makeEmpty(b)
        }
        if (newGenerator) {
          for (l <- 1 to j) {
            sarr(l) = h :: sarr(l)
            uarr(l) = uarr(l).addingGenerator(h)
          }
        }
      }
    }

    case class BSGSGroup(sarr: Seq[Seq[UnderlyingElement]], uarr: Vector[Transversal]) extends BSGSBasics {
      type Group = BSGSGroup
      type Element = BSGSGroupElement
      def make(bvec: Vector[Domain]) = BSGSGroupElement(bvec)
      override def toString = "BSGSGroup of order " + order + " and degree " + degree
      case class BSGSGroupElement(bvec: Vector[Domain]) extends BSGSElement {
        self: Element =>
        val group = BSGSGroup.this
      }
    }

    trait BSGSBasics extends PermutationGroup  {
      type Group <: BSGSBasics
      type Element <: BSGSElement

      val sarr: Seq[Iterable[g.Element]]
      val uarr: Seq[Transversal]
      def generators = sarr.flatten.map(sift(_)._1)
      def make(bvec: Vector[Domain]): Element
      def identity = make(uarr.map(_.beta).toVector)
      val degree = g.degree

      trait BSGSElement extends Permutation {
        self: Element =>
        val bvec: Vector[Domain]
        def underlying: UnderlyingElement = {
          var el = g.identity
          for ((b, i) <- bvec.zipWithIndex)
            el = uarr(i)(b).inverse * el
          el
        }
        def equal(that: Element) = bvec == that.bvec
        def isIdentity = bvec.sameElements(uarr.map(_.beta))
        def compare(that: Element) = underlying.compare(that.underlying)
        def images = underlying.images
        def image(e: Domain) = underlying.image(e)
        def assertValid =
          (uarr zip bvec).map { case (u,b) => assert(u.isDefinedAt(b)) }
        def inverse = {
          val basisPlace = Map.empty[Domain, Int] ++
          (for ((u, i) <- uarr.zipWithIndex) yield (u.beta, i))
          val p = SymmetricGroup(length).make(bvec.map(basisPlace(_))).inverse.img
          make(p.map(uarr(_).beta))
        }
        def *(that: Element) = {
          val basisPlace = Map.empty[Domain, Int] ++
          (for ((u, i) <- uarr.zipWithIndex) yield (u.beta, i))
          val g = SymmetricGroup(length)
          val p1 = g.make(bvec.map(basisPlace(_)))
          val p2 = g.make(bvec.map(basisPlace(_)))
          val p3 = p1 * p2
          make(p3.img.map(uarr(_).beta))
        }
      }
      /** Length of the stabilizer chain. */
      def length = uarr.length
      def contains(el: UnderlyingElement) = sift(el)._2.isIdentity
      def contains(el: Element) = true
      def order: BigInt = (BigInt(1) /: uarr)( (p:BigInt, u:Transversal) => u.size*p)
      
      def randomElement = make(uarr.map(_.random._1).toVector)

      def sift(el: UnderlyingElement, i: Int = 0, ind: ArrayBuffer[Domain] = ArrayBuffer(uarr.map(_.beta):_*)): (Element, UnderlyingElement, Int) = {
        // we left the base? exit
        if (i >= length)
          return (make(ind.toVector), el, length)
        val b = el.image(uarr(i).beta)
        // is the image of the current base element in the transversal ?
        if (!uarr(i).isDefinedAt(b))
          // if not, exit
          return (make(ind.toVector), el, i)
        ind(i) = b
        // we fixed the current base element, on to the next one
        sift(el * uarr(i)(b), i + 1, ind)
      }

      /** Checks this BSGS construction for consistency. TODO: do an actual check. */
      def assertValid {
        // verify that elements in transversals only move points they are supposed to move
        for (i <- 1 until length) {
          for (u <- uarr(i).iterator.map(_._2); j <- 0 until i) {
            assert(u.image(uarr(j).beta) == uarr(j).beta)
          }
        }
      }

      /** Iterates through all the elements of the group, by describing any element
        * of the group as a product of members of transversals.
        */
      def elements = {
        object MyIterable extends Iterable[Element] {
          def iterator = {
            def iter(i: Int): Iterator[List[Domain]] =
              if (i == length) List(Nil).iterator else
                for(g <- uarr(i).iterator; h <- iter(i+1)) yield g._1 :: h
            iter(0).map(_.toVector).map(make(_))
          }
        }
        MyIterable
      }

    }

  }

}
