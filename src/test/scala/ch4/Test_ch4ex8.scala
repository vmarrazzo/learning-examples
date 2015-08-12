package ch4

import org.scalatest.FlatSpec
import org.scalatest.MustMatchers
import scala.concurrent.{ Await, Future, ExecutionContext, Promise}
import scala.util.{ Failure, Success}
import ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.concurrent.duration._
import scala.concurrent.duration.Duration

import org.learningconcurrency.exercises.ch4.ch4ex8._

class Test_ch4ex8 extends FlatSpec
    with MustMatchers {

  behavior of "ch4ex8"
  
  /**
   * Test Predicate
   */
  def startWithUpperCase: String => Boolean = s => s(0).isUpper

  /**
   * 
   */
  it should "works into simple case" in {

    /**
     * Case one -> Promise[S] affect Promise[P]
     */
    val p = Promise[Boolean]
    
    val u : Promise[String] = p.compose(startWithUpperCase)
    
    p.isCompleted must be (false)
    
    u success("bello")
    
    Await.result(p.future, 10 millis) must be (false)
    
    /**
     * Case two -> Promise[S] affect Promise[P] (different variable)
     */
    val p2 = Promise[Boolean]
    
    val u2 = p2.compose(startWithUpperCase)
    
    p2.isCompleted must be (false)
    
    u2 success("Nello")
    
    Await.result(p2.future, 10 millis) must be (true)
    
    /**
     * Case three -> Promise[S] does not affect Promise[P] because this one is alredy complete
     */
    val p3 = Promise[Boolean]
    
    val u3 = p3.compose(startWithUpperCase)
    
    p3.isCompleted must be (false)
    
    p3 success true
    
    u3 success("bello")
    
    Await.result(p3.future, 10 millis) must be (true)
  }
}