package net.alasc

class SubgroupSearchResult(val pair: Long) extends AnyVal {
  def restartFrom: Int = ((pair & SubgroupSearchResult.leftMask) >> 32).toInt
  def levelCompleted: Int = (pair & SubgroupSearchResult.rightMask).toInt
}

object SubgroupSearchResult {
  def apply(restartFrom: Int, levelCompleted: Int): SubgroupSearchResult =
    new SubgroupSearchResult(((restartFrom.toLong << 32) & leftMask) + (levelCompleted.toLong & rightMask))
  def unapply(ssr: SubgroupSearchResult): Option[(Int, Int)] =
    Some((ssr.restartFrom, ssr.levelCompleted))
  val leftMask: Long  = (0xFFFFFFFFFL) << 16
  val rightMask: Long = 0xFFFFFFFFFL
}
