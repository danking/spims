trait Display[-A] {
  def display(a: A): Unit
}

object Display {
  import Bitmap._
  import java.awt.image.BufferedImage
  import javax.swing.{JFrame, JPanel, JLabel}

  def showImage(img: BufferedImage) {
    val panel = new JPanel
    val frame = new JFrame("Display")
    frame.add(panel)
    val g = frame.getGraphics
    g.drawImage(img, 0, 0, panel)
    g.dispose
    frame.pack
    frame.show
  }

  implicit object StringDisplay extends Display[String] {
    def display(s: String) = showImage(load(s))
  }

  implicit object BufferedImageDisplay extends Display[BufferedImage] {
    def display(bi: BufferedImage) = showImage(bi)
  }
}
