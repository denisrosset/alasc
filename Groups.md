About groups in Alasc
=====================

There are two concepts related to groups in Alasc. Alasc can work with elements of arbitrary type `G`,
as long as Alasc knows how:

1) how to check whether `g1: G` and `g2: G` are equal,

2) how to combine two elements `g1: G, g2: G` together,

3) what is the identity element of `G`,

4) how to compute the inverse of any `g: G`.

Property 1) is provided by a `spire.algebra.Eq[A]` instance, while properties 2-4 are provided by a `spire.algebra.Group[A]`
instance. Additionally, the group laws have to be met, see the definition of `spire.algebra.Group`. This fact can, and should be
checked using the Discipline laws provided by Spire.

When these conditions are met, we can say that the set of all the instances of type `G` form a group. Of course, the set of
all instances of `G` will never be represented in practice by a collection in Scala; it does not make sense, either, to talk
about the `Set[Int]` of all possible integers.

However, we can define and work with smaller sets of elements of `G` that are closed under the combination and inverse operations.
These sets are then groups of their own; in Alasc, there are of type `Grp[G]`. All the instances of `Grp[G]` in Alasc should
contain only a finite number of elements. 

When no additional structure is provided, Alasc works with `Grp[G]` by storing a copy of all their elements. This is the
blackbox approach, which used in practice only to check the correctness of the other approaches implemented in Alasc.

In that case, use the `blackbox` package.

Implementation of bases and strong generating sets
--------------------------------------------------

Alasc currently provides a simple implementation of BSGS algorithms. Currently:

- it stores explicit transversals instead of Schreier vectors,

- it has implementations of several base change algorithms (recomputation from scratch, base swap, base swap
with conjugation),

- it implements deterministic and randomized variants of the Schreier-Sims and base swap algorithms,

- it does not implement partition backtracking.

These algorithms are provided in the `prep` package. Compared to other libraries/CAS, Alasc can work with arbitrary elements `G`, 
as long as a faithful permutation representation (of type `FaithfulPRep`) can be provided for any set of generators of type `G`.

When constructing a `PGrp[G]` from a set of generators, Alasc will perform the following tasks:

- look for a `PRepBuilder[G]` implicit instance,

- use the `PRepBuilder[G]` instance with the set of generators to construct a `FaithfulPRep[G]`,

- use this faithful permutation representation to construct a stabilizer chain for the group formed by the generators.

How then can you use the BSGS algorithms yourself ?

- use the provided `net.alasc.perms.Perm` type, that represents permutations. All the required implicits are provided
  automatically.

- your type `G` has an unique faithful permutation representation of constant dimension. 
  Implement `FaithfulPermutationAction[G]` and create an instance of `net.alasc.prep.UniquePRepBuilder`.

- your type `G` represents a permutation exactly. Implement `net.alasc.algebra.Permutation[G]`; then use 
  `net.alasc.perms.PermutationRepBuilder` to provide a simple the `PRepBuilder[G]` instance that Alasc needs.

- the faithful permutation representation varies depending on the set of generators. Implement `PRepBuilder[G]` for your
  type. Look at `net.alasc.std.PRepBuilderProduct2`.
