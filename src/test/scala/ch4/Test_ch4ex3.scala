package ch4

import org.scalatest.FlatSpec
import org.scalatest.MustMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._
import scala.concurrent.{ Await, blocking, ExecutionContext, Future }
import scala.concurrent.duration._
import scala.language.postfixOps
import ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }

class Test_ch4ex3 extends FlatSpec
    with MustMatchers
    with GeneratorDrivenPropertyChecks {

  /**
   * Generator of Future[Unit]
   */
  //  val genFuture: Gen[Future[Unit]] = for {
  //    pass <- arbBool.arbitrary
  //  } yield {
  //    if (!pass) Future.successful[Unit] {}
  //    else Future.failed(new IllegalArgumentException)
  //  }

  val passedFuture = Future.successful[Unit] {}

  val failedFuture = Future.failed(new IllegalArgumentException)

  /**
   * Predicates
   */
  val passPredicate: Unit => Boolean = Unit => {
    true
  }
  val failPredicate: Unit => Boolean = Unit => {
    false
  }

  behavior of "ch4ex3"

  it should "work into simple case" in {

    val passedFuture = Future.successful[Unit] {}
    val failedFuture = Future.failed(new Exception)

    import org.learningconcurrency.exercises.ch4.ch4ex3._

    Await.result(passedFuture.exists { passPredicate }, 1 seconds) must be(true)
    Await.result(passedFuture.exists { failPredicate }, 1 seconds) must be(false)
    Await.result(failedFuture.exists { passPredicate }, 1 seconds) must be(false)
    Await.result(failedFuture.exists { failPredicate }, 1 seconds) must be(false)
  }

  it should "use the extension of Future correctly" in {

    forAll((arbBool.arbitrary, "Is Future Passed?"), (arbBool.arbitrary, "Chosen Predicate")) {
      (futureIsPassed: Boolean, predicate: Boolean) =>
        {

          import org.learningconcurrency.exercises.ch4.ch4ex3._

          val future = if (futureIsPassed) passedFuture else failedFuture

          val expected = Future {

            var resp = false

            try {
              Await.result(future, 10 millis)
              resp = predicate
            } catch {
              case t: Throwable => resp = false
            }

            resp
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