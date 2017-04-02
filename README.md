AlaSc, it's Scala permuted!
===========================

[![Join the chat at https://gitter.im/denisrosset-alasc/Lobby](https://badges.gitter.im/denisrosset-alasc/Lobby.svg)](https://gitter.im/denisrosset-alasc/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

AlaSc is a Computational Group Theory library aiming to implement
permutation group algorithms in Scala. The library implements the
deterministic and randomized Schreier-Sims algorithms using
explicit transversals, along with a few helpers.

It also provides a `Grp` class which can be used to explore any finite
group provided a faithful action is provided.

The library is by no means complete, and the API is unstable for now.

Similar projects include:
* The open source C++ library [PermLib](http://www.math.uni-rostock.de/~rehn/software/permlib.html)
* The open source C program [GAP System](http://www.gap-system.org/)
* The closed source program [Magma](http://magma.maths.usyd.edu.au/magma/)

This software is licensed under the terms of the [GNU Public License version 3](http://www.gnu.org/licenses/gpl-3.0.html).

YourKit is kindly supporting AlaSc open source project with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:
* [YourKit Java Profiler](http://www.yourkit.com/download/) and
* [YourKit .NET Profiler](http://www.yourkit.com/dotnet/download/)

```
Welcome to Scala version 2.11.2 (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_45).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import net.alasc.finite._; import net.alasc.perms._; import net.alasc.perms.default._; import net.alasc.syntax.all._; import net.alasc.std.any._; import spire.implicits._

scala> val M11 = Grp(Perm(1,2,3,4,5,6,7,8,9,10,11), Perm(3,7,11,8)(4,10,5,6))
M11: net.alasc.math.Grp[net.alasc.math.Perm] = Grp(Perm(1,2,3,4,5,6,7,8,9,10,11), Perm(3,7,11,8)(4,10,5,6))

scala> M11.setwiseStabilizer(2,9)
res0: net.alasc.math.Grp[net.alasc.math.Perm] = Grp(Perm(1,8)(2,9)(3,4)(5,10), Perm(1,10)(3,4)(5,8)(7,11), Perm(1,6,3,7)(4,5,8,10), Perm(1,7,3,6)(4,10,8,5), Perm(1,10,3,5)(4,6,8,7)) of order 144
```
