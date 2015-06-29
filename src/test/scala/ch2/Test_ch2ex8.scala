package ch2

import scala.concurrent.duration._
import org.scalatest.FlatSpec
import org.scalatest.concurrent.Timeouts
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import ch2ex8.{ PriorityTaskPool, AbstractPriorityTaskPool }
import scala.language.postfixOps
import org.learningconcurrency.log

/**
 * trait with shared test resources
 */
trait AbstractTest extends Timeouts with GeneratorDrivenPropertyChecks {

  /**
   * to be override
   */
  var numberOfTasks: Int

  /**
   * Generator of priority task
   */
  val genTask = for {
    priority <- Gen.choose(1, 10)
    string <- Gen.listOfN(10, Gen.alphaChar).map(_.mkString)
  } yield (priority, string)

  /**
   *
   */
  def coreTest[T <: AbstractPriorityTaskPool](underTest: T): Unit = {

    forAll(Gen.listOfN(numberOfTasks, genTask)) { (listTasks: List[(Int, String)]) =>
      {

        log(s"### Input -> $listTasks")

        /**
         * Create a mutable ArrayBuffer for final check
         */
        import scala.collection.mutable._
        val checkList = ArrayBuffer.empty[String]

        /**
         * Worker body method
         */
        def body(value: String): Unit = checkList.synchronized {
          log(s"write on shared resource ${value}")
          checkList += value
        }

        /**
         * Invoke the "asynchronous" method and fill with mimic real traffic
         */
        for (t <- listTasks) underTest.asynchronous(t._1)(body(t._2))

        failAfter(2 seconds) {

          // not necessary synchronize
          while (underTest.tasks.size > 0)
            Thread.sleep(50)
        }

        log(s"*** Output --> $checkList")

        /**
         * Verification for this test depends reciprocal position among high priority and low priority
         * In particular manner when high priority tasks occur at end of generation list.
         *
         * So no verification is proposed but only a print log
         */

        implicit val ord = new Ordering[(Int, String)] {
          def compare(a: (Int, String), b: (Int, String)) = b._1 - a._1
        }

        log(s"/// Expected sorted --> ${listTasks.sortBy[Int](_._1).map(_._2)}")
      }
    }
  }

}

/**
 * Concrete test class
 */
class Test_ch2ex8 extends FlatSpec with AbstractTest {

  behavior of "ch2ex8"

  /**
   * number of tasks to be generated
   */
  override var numberOfTasks = 5

  it should "works using scalacheck" in {

    coreTest[PriorityTaskPool](new PriorityTaskPool)
  }
}