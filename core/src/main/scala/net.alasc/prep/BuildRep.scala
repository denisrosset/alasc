package net.alasc
package prep

import net.alasc.finite.Rep

trait BuiltRep[G] extends Rep[G] {

  type B <: RepBuilder[G] with Singleton

  implicit val builder: B

}

object BuiltRep {

  type In[B0 <: RepBuilder[G] with Singleton, G] = BuiltRep[G] { type B = B0 }

}
