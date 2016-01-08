Style guide
===========


- methods giving the number of X are written `nX`, example: `nElements`
- variables/arguments representing type class instances are written
  using the type class name in lower camel case, possibly with the type parameter
  appended. Example: `val group: Group[A]`, or `val groupA: Group[A]`

- for the order of method parameters, when the definition of `argY` depends on the
  value of `argX`, the order of parameters is `argX` then `argY`


Singleton types
===============

- avoid mixing singleton types and default parameters in methods, see SI-9611

