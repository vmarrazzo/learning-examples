package ch3

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration._
import scala.language.postfixOps
import ch3.ch3ex2.TreiberStack
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.concurrent.Timeouts
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ MustMatchers, FlatSpec }

import org.learningconcurrency.log

//import org.learningconcurrency.ch3.{execute => callExecutor}

import ch2.thread

import scala.collection.mutable.{ Set, Stack }

class Test_ch3ex2 extends FlatSpec
  with MustMatchers
  with Timeouts
  with GeneratorDrivenPropertyChecks {

  behavior of "ch3ex2"

  /**
   * Generator section
   */

  /**
   * It is the generic action performed by thread
   */
  sealed trait ActionType

  /**
   * Push test action
   */
  case object PushAction extends ActionType

  /**
   * Pop test action
   */
  case object PopAction extends ActionType

  /**
   * Random test matrix can in theory empty under test data structure so before real test fill with some data 
   */
  val setupStep = 100

  /**
   * Number of thread 
   */
  val numbThread = 10

  /**
   * Number of step for each thread 
   */
  val threadStep = 10

  /**
   * Thread action matrix generator
   *
   * @return a test matrix with one row of actions for each thread
   */
  val generateActions = Gen.listOfN(numbThread,
    Gen.listOfN(threadStep,
      Gen.oneOf(PushAction, PopAction)) suchThat (_.length.equals(threadStep))) suchThat (_.length.equals(numbThread))

  /**
   * Customize the number of test into ScalaCheck environment
   */
  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 1, workers = 1)

  /**
   * Generator section - END
   */

  /**
   * Random number generator only to fill data structure
   */
  private val random = scala.util.Random

  /**
   * Used to track test action occurred
   *
   * @param testStep is the progressive index generated in concurrency
   * @param action describes the action type
   * @param value is the value obtained
   */
  case class TestActionLog(testStep: Int, action: ActionType, value: Int)

  /**
   * Runnable body for each thread involved into test
   */
  def runnable: (List[ActionType], TreiberStack[Int], AtomicInteger, Set[TestActionLog]) => Unit =

    (action: List[ActionType], tStack: TreiberStack[Int], testStepGenerator: AtomicInteger, testCheck: Set[TestActionLog]) => {

      var actionCount = 0

      for (i <- action) {

        Thread.sleep(random.nextInt(10) * 10)

        actionCount += 1

        i match {
          case PushAction => {
            val next = random.nextInt(100)
            tStack.push(next)
            log(s"action n. $actionCount executed ###PUSH### operation of value $next")

            testCheck += TestActionLog(testStepGenerator.incrementAndGet, PushAction, next)
          }
          case PopAction => {
            val fetch = tStack.pop
            log(s"action n. $actionCount executed ***POP*** operation of value $fetch")

            testCheck += TestActionLog(testStepGenerator.incrementAndGet, PopAction, fetch)
          }
        }
      }
    }

  /**
   * The main test case
   */
  it should "works with multi thread and Scala check" in {

    forAll((generateActions, "Threads action matrix")) {

      (actionArrays: List[List[ActionType]]) =>
        {

          /**
           * Under test object
           */
          val underTest = new TreiberStack[Int]

          /**
           * Used to reorder test data for verification steps
           */
          val testStepGenerator: AtomicInteger = new AtomicInteger(0)

          /**
           * Verification data structure
           */
          val testCheck = Set.empty[TestActionLog]

          /**
           * Fill the under test stack with some data
           */
          for (i <- 1 to setupStep) {
            val next = random.nextInt(100)
            underTest.push(next)

            log(s"pre-setup $i push operation of value $next")
            val timestamp = System.currentTimeMillis

            testCheck += TestActionLog(testStepGenerator.incrementAndGet, PushAction, next)
          }

          /**
           * Timed area
           */

          failAfter(2 seconds) {

            implicit val printLog: Boolean = false

            val group = for (actions <- (actionArrays zipWithIndex)) yield thread(s"Sample${actions._2}", runnable(actions._1, underTest, testStepGenerator, testCheck))

            for (t <- group) t.start

            var exit = false

            while (!exit) {
              exit = group.map(_.isAlive).filter((b: Boolean) => b).isEmpty
            }
          }

          /**
           * Timed area - END
           */

          /**
           * Verification
           */

          /**
           * Build a stack based on test log obtained
           */
          val testStack: Stack[Int] = {

            val resp = new Stack[Int]
            val actionLog = testCheck.toSeq.sortBy(_.testStep)

            for (act <- actionLog)
              act match {
                case TestActionLog(_, PushAction, value) => resp.push(value)
                case TestActionLog(_, PopAction, value) => resp.pop
              }

            resp
          }

          /**
           * Verify built stack if has expected size
           */
          testStack must have size {

            var totalAction = 0
            var pushAction = 0

            for (act <- actionArrays) {
              totalAction += act.length
              pushAction += act.filter(_ eq PushAction).length
            }

            setupStep + 2 * pushAction - totalAction
          }

          /**
           * verify that each element into built stack is inside underTest
           */
          for (i <- testStack) {

            val popped = underTest.pop

            withClue(s"underTest -> $underTest\n testStack -> $testStack : ") {
              popped must be(i)
            }
            log(s"$popped is correct")
          }

          /**
           * another pop on underTest and we obtain exception
           */
          try {
            underTest.pop

            fail("under test stack must be empty now!")
          } catch {
            case _: Exception => assert(true)
          }

          /**
           * Verification - END
           */

        }

    }

  } // end main test case

}
