import java.awt.image.{BufferedImage, ConvolveOp, DataBufferByte}
import java.io.{File, IOException}
import javax.imageio._
import javax.imageio.event.IIOReadWarningListener

object Bitmap {
  def apply(bi: BufferedImage) = {
    val (w, h) = (bi.getWidth, bi.getHeight)
    val fmt = if (bi.getType == BufferedImage.TYPE_BYTE_GRAY) Grey else Color
    val rgb = bi.getRGB(0, 0, w, h, null, 0, w)
    new Bitmap(w, h, rgb, fmt)
  }

  def load(path: String) = {
    val file = new File(path)

    if (!file.exists())
      throw new IOException("Cannot find file: " + path)

    ImageIO.read(file)
  }

  def save(img: BufferedImage, fileType: String, path: String) {
    ImageIO.write(img, fileType, new File(path))
  }

  def deepCopy(img: BufferedImage) = {
    val cm = img.getColorModel
    val isAlphaPremultiplied = cm.isAlphaPremultiplied
    val raster = img.copyData(null)
    new BufferedImage(cm, raster, isAlphaPremultiplied, null)
  }

  def subtract(bmp1: Bitmap, bmp2: Bitmap) = {
    val subbedRGB = (bmp1.rgb zip bmp2.rgb) map { case (p1, p2) => Math.abs(p1 - p2) }
    new Bitmap(bmp1.width, bmp1.height, subbedRGB, Grey)
  }

  def gaussianBlur(img: BufferedImage, radius: Float) = {
    val kern = GaussianBlur.makeKernel(radius)
    val op = new ConvolveOp(kern, ConvolveOp.EDGE_NO_OP, null)
    val dest = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    op.filter(img, dest)
  }
}

abstract class Format
case object Color extends Format
case object Grey extends Format
class Bitmap(val width: Int, val height: Int, val rgb: Array[Int], val fmt: Format) {
  def this() = this(0, 0, new Array[Int](0), Color)
  def this(width: Int, height: Int) = this(width, height, new Array[Int](width * height), Color)

  def apply(x: Int, y: Int) = rgb(y * width + x)

  def getNoAlpha(x: Int, y: Int) = rgb(y * width + x) & 0xFFFFFF

  def getGrey(x: Int, y: Int) = rgb(y * width + x) & 0xFF

  def toBufferedImage = {
    val imgType = fmt match {
      case Grey => BufferedImage.TYPE_BYTE_GRAY
      case _ => BufferedImage.TYPE_INT_ARGB
    }
    val bi = new BufferedImage(width, height, imgType)
    bi.setRGB(0, 0, width, height,  rgb, 0, width)
    bi
  }
}
