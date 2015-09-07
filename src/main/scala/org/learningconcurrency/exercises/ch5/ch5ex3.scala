package org.learningconcurrency.exercises.ch5

import breeze.math.Complex
import scala.swing.{ BorderPanel, Component, MainFrame }
import java.awt.{ Color, Dimension, Graphics2D }

object ch5ex3 {

  /**
   * Input of problem :
   * - a square area described for highest (up/right) and lowest (down/left) points
   * - final plot size in pixels
   */
  val high = Complex(1.0, 1.0)
  val low = Complex(-1.0, -1.0)

  /**
   * This values describe convergenceMap area resolution in pixel
   */
  val width: Int = 640
  val height: Int = 480

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
   * Main
   */
  def main(args: Array[String]): Unit = {

    // the last "take" is to ensure that points are equals to expected pixels
    val xrange = (low.real to high.real by (high.real - low.real) / width).take(width)
    val yrange = (low.imag to high.imag by (high.imag - low.imag) / height).take(height)

    /**
     * it generates a set pairs that correlate :
     * - Complex coordinate for mathematical calculation
     * - Pixel coordinate for plotting
     */
    val complex2PixelSeq = for {
      (x, xpixel) <- xrange.zipWithIndex
      (y, ypixel) <- yrange.reverse.zipWithIndex // in that manner left most corner is starting point
    } yield (Complex(x, y), (xpixel, ypixel))

    // Map from Complex plane to Pixel plane
    val complex2Pixel: Map[Complex, (Int, Int)] = complex2PixelSeq.toMap

    // the complex point for mathematical calculation
    val complexPlanCoordinate = complex2Pixel.keys.toVector

    // This map contains the coordinate according convergence (None) and divergence (Some(s))
    val convergenceMap: Map[Complex, Option[Int]] = {
      for {
        point <- complexPlanCoordinate
      } yield (point -> indexOfConv(point))
    }.toMap

    val toBeColored = convergenceMap.filter { case ((k, v)) => v != None }
    val toBeBlack = convergenceMap.filter { case ((k, v)) => v == None }.keys // no added value with map

    println(s"Plot resoution ($width,$height)")
    println(s"xrange points -> ${xrange.size}")
    println(s"yrange points -> ${yrange.size}")
    println(s"Total number of expected points -> ${width * height}")
    println(s"Obtained points -> ${convergenceMap.size}")
    println(s"Point that diverge and will be colored -> ${toBeColored.size}")

    /*
    import java.io._
    val pw = new PrintWriter(new File("toBeColored.txt"))
    toBeColored.toList.sortBy { { case (c, o) => c } }.foreach { { case (c, o) => pw.write(s"Point -> $c\tValue -> $o\n") } }
    pw.close
    */

    val mFrame = new MainFrame {
      title = "ch5x3"
      preferredSize = new Dimension(width, height)
      visible = true
      centerOnScreen
      contents = new BorderPanel {
        add(new Component {
          override def paintComponent(g: Graphics2D) {
            toBeBlack foreach {
              case c: Complex => complex2Pixel.get(c) match {
                case Some((x, y)) => g.drawLine(x, y, x, y)
                case None => ??? // Impossible condition
              }
            }
          }
        }, BorderPanel.Position.Center)
      }
    } // end main frame

    Thread.sleep(2000)

    mFrame.dispose
  }
}