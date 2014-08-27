package net.alasc.coll

trait Keyable[-K, A] extends Any {
  def apply(key: K): A
}
