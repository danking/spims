trait Display[-A] {
  def display(a: A): Unit
}

object Display {
  import Bitmap._
  import java.awt.image.BufferedImage
  import javax.swing.{JFrame, JPanel, JLabel, ImageIcon}

  def showImage(img: BufferedImage) {
    val label = new JLabel(new ImageIcon(img))
    val panel = new JPanel
    val frame = new JFrame("Display")
    panel.add(label)
    frame.add(panel)
    /*frame.add(panel)
    val g = frame.getGraphics
    g.drawImage(img, 0, 0, panel)
    g.dispose*/
    frame.pack
    frame.show
  }

  def display[A](a: A)(implicit d: Display[A]) { d.display(a) }

  implicit object StringDisplay extends Display[String] {
    def display(s: String) = showImage(load(s))
  }

  implicit object BufferedImageDisplay extends Display[BufferedImage] {
    def display(bi: BufferedImage) = showImage(bi)
  }

  implicit object BitmapDisplay extends Display[Bitmap] {
    def display(bmp: Bitmap) = showImage(bmp.toBufferedImage)
  }
}
