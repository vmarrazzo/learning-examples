package org.learningconcurrency.exercises.ch4

import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Success, Failure }
import ExecutionContext.Implicits.global

object ch4ex4 {

  implicit class FutureOps[T](val self: Future[T]) {

    def exists(p: T => Boolean): Future[Boolean] = {

      val promise = Promise[Boolean]

      self onComplete {
        case Success(value) => promise success p(value)
        case Failure(_)     => promise success (false)
      }

      promise.future
    }
  }

}