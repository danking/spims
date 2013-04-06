import Paramater._

object Algorithm extends Logging{

  def findPatternInSource(source: Bitmap, pattern: Bitmap) = {
    for (tly <- 0 until source.height - pattern.height + 1) {
      for (tlx <- 0 until source.width - pattern.width + 1) {
        placeAtAndCompare(source, pattern, tly, tlx)
      }
    }
  }

  def placeAtAndCompare(source: Bitmap, pattern: Bitmap, tly: Int, tlx: Int) = {
    var totalDif = 0
    var avgDif = 0
    (for {
      y <- 0 until pattern.height
      x <- 0 until pattern.width
      if avgDif < Paramaters.AVG_DIF_TOL
      } yield {
        debug("totalDif = " + toString(totalDif))
        debug("avgDif = " + toString(avgDif))
        val dif = Math.abs(pattern(x, y) - source(x + tlx, y + tly))
        val size = pattern.height * pattern.width
        totalDif = totalDif + dif
        avgDif = totalDif/size
        preMatch(x, y, avgDif)
        }).toArray
  }
}
