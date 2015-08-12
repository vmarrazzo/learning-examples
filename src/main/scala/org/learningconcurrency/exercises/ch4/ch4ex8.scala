package org.learningconcurrency.exercises.ch4

import scala.concurrent.{ Future, ExecutionContext, Promise}
import scala.util.{ Failure, Success}
import ExecutionContext.Implicits.global

object ch4ex8 {
  
  implicit class PromiseOps[T](val self: Promise[T]) {
  
    def compose[S](f: S => T): Promise[S] = {
      
      val s = Promise[S]
      
      s.future onComplete {
        case Success(value) => if (!self.isCompleted) self success f(value)
        case Failure(ex) => if (!self.isCompleted) self failure(ex)
      }
      
      s
    }
  }
}