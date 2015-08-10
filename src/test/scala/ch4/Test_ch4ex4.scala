package ch4

import org.scalatest.FlatSpec
import org.scalatest.MustMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._
import scala.concurrent.{ Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.language.postfixOps
import ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }

import org.learningconcurrency.exercises.ch4.ch4ex4._

class Test_ch4ex4 extends FlatSpec
    with MustMatchers
    with GeneratorDrivenPropertyChecks {

  /**
   * Always passed future
   */
  val passedFuture = Future.successful[Unit] {}

  /**
   * Always failed future
   */
  val failedFuture = Future.failed(new IllegalArgumentException)

  /**
   * Always true predicate
   */
  val passPredicate: Unit => Boolean = Unit => {
    true
  }
  
  /**
   * Always false predicate
   */
  val failPredicate: Unit => Boolean = Unit => {
    false
  }

  behavior of "ch4ex4"

  it should "work into simple case" in {

    Await.result(passedFuture.exists { passPredicate }, 1 seconds) must be(true)
    Await.result(passedFuture.exists { failPredicate }, 1 seconds) must be(false)
    Await.result(failedFuture.exists { passPredicate }, 1 seconds) must be(false)
    Await.result(failedFuture.exists { failPredicate }, 1 seconds) must be(false)
  }

  it should "use the extension of Future correctly" in {

    forAll((arbBool.arbitrary, "Is Future Passed?"), (arbBool.arbitrary, "Chosen Predicate")) {
      (futureIsPassed: Boolean, predicate: Boolean) =>
        {
          val future = if (futureIsPassed) passedFuture else failedFuture

          val expected = {

            val p = Promise[Boolean]()
            
            future onComplete {
              case Success(value) => p success predicate
              case Failure(_) => p success false
            }

            p.future
          }

          Await.result(future.exists {
            if (predicate)
              passPredicate
            else
              failPredicate
          }, 10 millis) must be(Await.result(expected, 10 millis))
        }
    }
  }

}