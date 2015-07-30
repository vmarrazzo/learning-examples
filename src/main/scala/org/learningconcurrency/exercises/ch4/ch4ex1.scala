package org.learningconcurrency.exercises.ch4

import java.util.{ Timer, TimerTask }
import scala.concurrent.{ Await, ExecutionContext, Future, Promise, TimeoutException}
import scala.concurrent.duration._
import scala.concurrent.duration.Duration
import scala.io.{ Source, StdIn }
import scala.util.{ Success, Failure }
import scala.language.postfixOps
import ExecutionContext.Implicits.global

import org.learningconcurrency.ch4.PromisesCancellation._

object ch4ex1 {

  /**
   * Timeout Future
   *
   * I used a custom version that carry-in the "timer"
   */

  def timeout(t: Long): Future[Unit] = {

    val p = Promise[Unit]
    val timer = new Timer(true)

    timer.schedule(new TimerTask {
      def run() = {
        p failure (new TimeoutException(s"Elapsed $t ms timeout!"))
        timer.cancel
      }
    }, t)
    p.future
  }

  /**
   *
   *
   * Core block of code
   *
   *
   */

  /**
   * Service Time consuming Future
   */

  def genServiceFuture(url: String): Future[String] = Future { Source.fromURL(url).mkString }

  /**
   * User interaction future
   */

  def userInteraction: Future[String] = Future {

    val defaultPrompt = """Insert URL or "exit" > """

    println()
    print(defaultPrompt)

    val line = StdIn.readLine

    line
  }

  /**
   * main method
   *
   * @param args
   */

  def main(args: Array[String]) {

    val awaitTimeout = 5 seconds
    val twoDotsDelay = 50 // ms

    var inputLine = ""

    do {

      inputLine = Await.result(userInteraction, Duration.Inf)

      if (!inputLine.equals("exit")) {

        /**
         * It's a valid string to be "evaluated"
         */

        val (cancel, future) = cancellable(cancel => {

          while (!cancel.isCompleted) {

            // here is requirement add a 50 ms between two dot
            Thread.sleep(twoDotsDelay)
            print(" . ")
          }

          println
        })

        /**
         * Triggering the service future with timeout to prepare which message to print on screen
         */

        val what2Print: Future[String] = {

          val p = Promise[String]

          Future.firstCompletedOf( genServiceFuture(inputLine) :: timeout(2000) :: Nil) onComplete {

            case Success(data) =>
              if (!cancel.isCompleted) {
                cancel success {}

                p success s"Data fetched : \n${data.toString.substring(0, 100)}...."
              }

            case Failure(error) =>
              if (!cancel.isCompleted) {
                cancel success {}

                p success s"Error occurs before complete : ${error.getMessage}"
              }

          }

          p.future
        }

        /**
         * Brush is used to avoid conflict in loop printing
         */

        val brush: Future[Unit] = Future {

          val send2Screen = Await.result( what2Print, awaitTimeout)

          // I cannot avoid to add this sleep because I have not resolution on single dot printing
          Thread.sleep(twoDotsDelay)

          println()
          println(send2Screen)
        }

        Await.ready( brush, awaitTimeout)

      } // end if "exit"

    } while (!inputLine.equals("exit"))
  }
}