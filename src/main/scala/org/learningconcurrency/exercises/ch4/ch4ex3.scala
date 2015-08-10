package org.learningconcurrency.exercises.ch4

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.Duration
import scala.util.{ Success, Failure }
import ExecutionContext.Implicits.global

object ch4ex3 {

  implicit class FutureOps[T](val self: Future[T]) {

    def exists(p: T => Boolean): Future[Boolean] = Future {

      var resp: Boolean = false

      try {
        resp = p(Await.result(self, Duration.Inf))
      } catch {
        case t: Throwable => resp = false
      }

      resp
    }
  }

}