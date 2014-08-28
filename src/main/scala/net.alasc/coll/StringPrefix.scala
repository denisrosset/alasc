package net.alasc.coll

trait StringPrefix {
  self =>
  def stringPrefix : String = {
    var string = self.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }
}
