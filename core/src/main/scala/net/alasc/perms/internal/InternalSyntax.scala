package net.alasc.perms.internal

trait InternalSyntax {

  implicit def genPrmOps(lhs: GenPrm): GenPrmOps = new GenPrmOps(lhs)
  implicit def prmOps(lhs: Prm): PrmOps = new PrmOps(lhs)
  implicit def mutPrmOps[I <: XInt](lhs: MutPrm[I]): MutPrmOps[I] = new MutPrmOps[I](lhs)

}
