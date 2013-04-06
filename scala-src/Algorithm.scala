import Paramater

object Algorithm extends Logging{
	def findPatternInSource(source: Bitmap, pattern: Bitmap) = {
    for (tly <- 0 until source.height - pattern.height + 1) {
      for (tlx <- 0 until source.width - pattern.width + 1) {
        placeAtAndCompare(source, pattern, tly, tlx)
      }
    }
  }
  def placeAtAndCompare(source: Bitmap, pattern: Bitmap, tly: Int, tlx: Int) = {
    val totalDif = 0
    val avgDif = 0
    for (y <- 0 until pattern.height if avgDif < Paramaters.AVG_DIF_TOL) {
      for (x <- 0 until pattern.width if avgDif < Parameters.AVG_DIF_TOL) {
        Logging.debug("total-dif = " + toString(totalDif))
        Logging.debug("avg-dif =" + toString(avgDif))
        val dif = Math.abs(pattern(x, y) - source(x + tlx, y + tly))
        val size = pattern.height * pattern.width
        totalDif += dif
        avgDif = totalDif/size 
      }
    }
  }
}