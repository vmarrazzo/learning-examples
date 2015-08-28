package org.learningconcurrency.exercises.ch5

import org.learningconcurrency.ch5._

import breeze.stats.distributions.Uniform
import breeze.linalg._
import breeze.plot.{ Figure, plot }

object ch5ex2 {

  /**
   * Uniform distribution used to generate "events"
   */
  private lazy val uniform = breeze.stats.distributions.Uniform( 0, 1)

  /**
   * This method generate a string where the probability of a whitespace at each position is determined by a probability parameter.
   */
  private def generateString(probability: Double)(length: Int = 100): String = {

    val whiteSpace: Char = ' '
    val notWhiteSpace: Char = 'A'

    { for (i <- 1 to length) yield { if (uniform.get <= probability) whiteSpace else notWhiteSpace } }.mkString
  }

  def main(args: Array[String]) {

    val probs = List(0.2, 0.5, 0.7, 0.9)
    val size = 1e7.toInt

    import scala.collection.mutable.Buffer

    val seqtimes: Buffer[Double] = Buffer.empty
    val partimes: Buffer[Double] = Buffer.empty

    println(s"String size $size")
    
    for (probability <- probs) {
      val patternString = generateString(probability)(size).toVector

      var whiteSpaceSequential = 0
      var whiteSpaceParallel = 0

      val seqtime = timed( whiteSpaceSequential = patternString.count(_ == ' ') )

      println(s"Probability $probability Sequential $seqtime ms finds $whiteSpaceSequential")

      val partime = timed( whiteSpaceParallel = patternString.par.count(_ == ' ') )

      println(s"Probability $probability Parallel time $partime ms finds $whiteSpaceParallel")

      assert(whiteSpaceSequential == whiteSpaceParallel, "Something goes really bad!")
      assert(seqtime >= partime, "This time does not works!")
      
      seqtimes += seqtime
      partimes += partime
    }

    val figure = Figure("ch5ex2")
    val mainPlot = figure.subplot(0)
   
    mainPlot.title = "Count event occurrence Sequential/Parallel"
    
    mainPlot += plot( probs, seqtimes.toList, name = s"Sequential")
    mainPlot += plot( probs, partimes.toList, name = s"Parallel")

    mainPlot.setXAxisDecimalTickUnits
    
    mainPlot.xlabel = "Probability of event"
    mainPlot.ylabel = "Time spent to count (ms)"

    mainPlot.legend = true
  }
}