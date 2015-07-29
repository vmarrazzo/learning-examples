package org.learningconcurrency.exercises.ch4

import java.util.{ Timer, TimerTask }
import scala.concurrent._
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
   * Core block of code
   */
  def main(args: Array[String]) {

    var inputLine = ""

    do {

      inputLine = Await.result(userInteraction, Duration.Inf)

      if (!inputLine.equals("exit")) {

        /**
         * It's a valid string to be "evaluated"
         */

        val (cancel, future) = cancellable(cancel => {

          while (!cancel.isCompleted) {
            Thread.sleep(50)
            print(" . ")
          }

          println
        })

        /**
         * Triggering the service future with a timeout and prepare what to print on screen
         */

        val what2Print: Future[String] = {

          val p = Promise[String]

          Future.firstCompletedOf(genServiceFuture(inputLine) :: timeout(2000) :: Nil) onComplete {

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

        val brush: Future[Unit] = Future {

          var send2Screen = Await.result(what2Print, 10 seconds)

          Thread.sleep(50)

          println()
          println(send2Screen)
        }

        Await.ready(brush, 10 seconds)

      } // end if "exit"

    } while (!inputLine.equals("exit"))
  }
}