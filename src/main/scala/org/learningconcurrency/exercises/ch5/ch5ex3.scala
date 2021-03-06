package org.learningconcurrency.exercises.ch5

import breeze.math.Complex
import scala.swing.{ BorderPanel, Component, MainFrame }
import java.awt.{ Color, Dimension, Graphics2D, Image }
import java.awt.image.BufferedImage

import scala.collection._

import org.learningconcurrency.ch5._

object ch5ex3 {

  /**
   * Input of problem :
   * - a square area described for highest (up/right) and lowest (down/left) points
   * - final plot size in pixels
   */
  val high = Complex(2.0, 2.0)
  val low = Complex(-2.0, -2.0)

  /**
   * This values describe convergenceMap area resolution in pixel
   */
  val width: Int = 800
  val height: Int = 600

  /**
   * If a Mandelbrot set reach this number without diverge is convergenceMapted as belong it
   */
  val maxConvergenceThreshold = 100

  /**
   * Mandelbrot general function
   */
  def mandelbrotSet(zN_1: Complex, point: Complex): Complex = zN_1 * zN_1 + point

  /**
   * Divergence function
   */
  def diverge: Complex => Boolean =
    complex => {
      val abs = complex.abs
      abs.isNaN || abs >= 2
    }

  /**
   * It evaluates the convergence according Mandelbrot Set and maxConvergenceThreshold :
   * - None means that coordinate does not diverge into threshold
   * - Some(x) means how many steps are necessary to diverge
   */
  def indexOfConv(point: Complex): Option[Int] = {

    // Concrete Mandelbrot set generator with point
    def mGen: Complex => Complex = mandelbrotSet(_: Complex, point)

    // generate a stream according generator and max threshold
    val stream = Stream.iterate[Complex](point, maxConvergenceThreshold)(mGen)

    // evaluate convergence
    stream.find(diverge) match {
      case Some(index) => Some(stream.indexOf(index)) // diverge so will be convergenceMapted
      case None => None // never diverge -> black
    }
  }

  /**
   * It produces the rendered image for plotting
   */
  private def renderingImage( convergenceMap: GenMap[Complex, Option[Int]],
                              complex2Pixel: Map[Complex, (Int, Int)]): BufferedImage = {

    // Cap. Side effects in parallel operations
    // Improved using a sort of atomic action setColor and draw pixel together
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    def calculateColor(convIndex: Option[Int]): Color =
      convIndex match {
        case Some(iter) => {

          // iter is contained into (0,maxConvergenceThreshold)
          val c = 3 * math.log(iter + 1.0) / math.log(maxConvergenceThreshold)

          val (r, g, b) =
            if (c < 1)
              ((255 * c).toInt, 0, 0)
            else if (c < 2)
              (255, (255 * (c - 1)).toInt, 0)
            else
              (255, 255, (255 * (c - 2)).toInt)

          //import org.learningconcurrency._
          //log(s"Index -> ${iter} to ($r,$g,$b)")

          new Color(r, g, b)
        }
        case None => Color.BLACK
      }

    convergenceMap foreach {
      case (complex, indexConv) => {
        complex2Pixel.get(complex) match {
          case Some((x, y)) => image.setRGB(x, y, calculateColor(indexConv).getRGB)
          case None => ??? // Impossible condition
        }
      }
    }

    image
  }

  /**
   * It used to encapsulate plotting rendered image.
   */
  private def plotFrame(  image: BufferedImage,
                          pTitle: String = "ch5x3",
                          dispose: Boolean = true): Unit = {

    val mFrame = new MainFrame {

      title = pTitle
      preferredSize = new Dimension(width, height)
      visible = true
      centerOnScreen
      resizable = false

      contents = new BorderPanel {
        add(new Component {

          override def paintComponent(g: Graphics2D) {
            g.drawImage(image, 0, 0, null)
          }

        }, BorderPanel.Position.Center)
      }
    } // end main frame

    Thread.sleep(2000)

    import java.io.File
    import javax.imageio.ImageIO

    ImageIO.write(image, "png", new File(s"${pTitle}.png"))

    if (dispose)
      mFrame.dispose
  }

  /**
   *
   *
   * Main
   *
   *
   */
  def main(args: Array[String]): Unit = {

    println(s"Plot resolution ($width,$height)")
    println(s"Bottom left coordinate -> ${low}")
    println(s"Up right coordinate -> ${high}")

    // the last "take" is to ensure that points are equals to expected pixels
    val xrange = (low.real to high.real by (high.real - low.real) / width).take(width)
    val yrange = (low.imag to high.imag by (high.imag - low.imag) / height).take(height)

    /**
     * it generates a set pairs that correlate :
     * - Complex coordinate for mathematical calculation
     * - Pixel coordinate for plotting
     */
    val complex2PixelPairs = for {
      (x, xpixel) <- xrange.zipWithIndex
      (y, ypixel) <- yrange.reverse.zipWithIndex // in that manner left most corner is starting point
    } yield (Complex(x, y), (xpixel, ypixel))

    // Map from Complex plane to Pixel plane
    val complex2Pixel: Map[Complex, (Int, Int)] = complex2PixelPairs.toMap

    // the complex point for mathematical calculation
    val complexPlanCoordinate = complex2Pixel.keys.toVector

    // GenMap will cover both sequential and parallel data test

    var convergenceMap: GenMap[Complex, Option[Int]] = null
    var convergenceMapPar: GenMap[Complex, Option[Int]] = null

    val creationConvergenceMapSeq = timed {
      // This map contains the coordinate according convergence (None) and divergence (Some(s))
      convergenceMap = {
        for {
          point <- complexPlanCoordinate
        } yield (point -> indexOfConv(point))
      }.toMap
    }

    val creationConvergenceMapPar = timed {
      // This map contains the coordinate according convergence (None) and divergence (Some(s))
      convergenceMapPar = {
        for {
          point <- complexPlanCoordinate.par
        } yield (point -> indexOfConv(point))
      }.toMap
    }

    val divergeElemSeq = convergenceMap.filter { case ((k, v)) => v != None }.size
    val convergElemSeq = convergenceMap.filter { case ((k, v)) => v == None }.size

    val divergeElemPar = convergenceMapPar.filter { case ((k, v)) => v != None }.size
    val convergElemPar = convergenceMapPar.filter { case ((k, v)) => v == None }.size

    assert(divergeElemSeq == divergeElemPar, s"Divergence points does not match (Seq=$divergeElemSeq,Par=$divergeElemPar)")
    assert(convergElemSeq == convergElemPar, s"Convergence points does not match (Seq=$convergElemSeq,Par=$convergElemPar)")

    println("Convergence sets (Sequentil and Parallel) are equals.")
    println(s"Convergence obtained sequentially $creationConvergenceMapSeq ms")
    println(s"Convergence obtained parallel $creationConvergenceMapPar ms")

    /*
    import java.io._
    val pw = new PrintWriter(new File("toBeColored.txt"))
    toBeColored.toList.sortBy { { case (c, o) => c } }.foreach { { case (c, o) => pw.write(s"Point -> $c\tValue -> $o\n") } }
    pw.close
    */

    var imageSeq: BufferedImage = null
    var imagePar: BufferedImage = null

    val renderingSeq = timed { imageSeq = renderingImage(convergenceMap, complex2Pixel) }
    val renderingPar = timed { imagePar = renderingImage(convergenceMapPar, complex2Pixel) }

    assert((imageSeq.getWidth == imagePar.getWidth) && (imageSeq.getHeight == imagePar.getHeight), s"Image size does not match Seq=(${imageSeq.getWidth},${imageSeq.getHeight}) Par=(${imagePar.getWidth},${imagePar.getHeight})")

    for (x <- 0 until imageSeq.getWidth)
      for (y <- 0 until imageSeq.getHeight)
        assert(imageSeq.getRGB(x, y) == imagePar.getRGB(x, y), s"Pixel ($x,$y) does not match!")

    println("Rendered images match pixel by pixel")
    println(s"Rendered sequentially $renderingSeq ms")
    println(s"Rendered parallel $renderingPar ms")

    plotFrame(imageSeq, "ch5x3 Sequential")
    plotFrame(imagePar, "ch5x3 Parallel")
  }
}
