import java.awt.image.Kernel

object GaussianBlur {
// TODO move gaussianBlur fuction from GrayBufferedImage.scala to here
// Also want to maybe implement a new COnvolveOp
  def makeKernel(sigma: Float) = {
    val radius = sigma * 3
    val r = Math.ceil(radius).toInt
    var rows = r * 2 + 1
    val matrix = new Array[Float](rows)
    val sigma22 = 2 * sigma * sigma
    val sigmaPi2 = 2 * Math.Pi * sigma
    val sqrtSigmaPi2 = Math.sqrt(sigmaPi2).toFloat
    val radius2 = radius * radius
    var total = 0.toFloat
    var index = 0
    var row = -r
    while (row <= r) {
      val distance = row * row
      if (distance > radius2)
        matrix(index) = 0
      else
        matrix(index) = Math.exp(-(distance) / sigma22).toFloat / sqrtSigmaPi2
      total = total + matrix(index)
      index = index + 1
      row = row + 1
    }

    var i = 0
    while (i < rows) {
      matrix(i) = matrix(i) / total
      i = i + 1
    }

    new Kernel(rows, 1, matrix)
  }
}
