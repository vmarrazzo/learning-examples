package ch2

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.concurrent.Timeouts
import org.learningconcurrency.log
import org.scalacheck.Gen
import ch2ex5._
import scala.language.postfixOps

class Test_ch2ex5 extends FlatSpec
  with MustMatchers
  with Timeouts
  with GeneratorDrivenPropertyChecks {

  behavior of "ch2ex5"

  val random = scala.util.Random

  def producerBody(numbIteration: Int, sleepBaseTime: Long, sync: SyncVar[Int], producedElements: ArrayBuffer[Int]): Unit = {

    for (i <- 0 until numbIteration) {

      val next = random.nextInt(15)
      sync.putWait(next)
      log(s"has produced -> $next")

      producedElements += next

      Thread.sleep(random.nextInt(10) * sleepBaseTime)
    }
  }

  def consumerBody(numbIteration: Int, sleepBaseTime: Long, sync: SyncVar[Int], consumedElements: ArrayBuffer[Int]): Unit = {

    for (i <- 0 until numbIteration) {

      val current = sync.getWait
      log(s"has consumed -> $current")

      consumedElements += current

      Thread.sleep(random.nextInt(10) * sleepBaseTime)
    }
  }

  /**
   * 
   */
  implicit override val generatorDrivenConfig = PropertyCheckConfig(workers = 50)

  it should "works using Scala Check properties" in {

    import org.scalacheck.Arbitrary._
    
    case class Scenario( consumerBefore: Boolean, numberExchanged: Int, producerBaseSleep: Int, consumerBaseSleep: Int)
    
    lazy val genScenario: Gen[Scenario] = for {
      consumerStartsBefore    <- arbBool.arbitrary
      numberExchanged         <- Gen.choose[Int](1, 15)
      producerBaseSleep       <- Gen.oneOf[Int](1, 10, 100)
      consumerBaseSleep       <- Gen.oneOf[Int](1, 10, 100)
    } yield Scenario( consumerStartsBefore, numberExchanged, producerBaseSleep, consumerBaseSleep)
    
    forAll(genScenario) { (scenario) => {
      
        log(scenario.toString)
      
        val sync: SyncVar[Int] = new SyncVar
        val producedElements: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
        val consumedElements: ArrayBuffer[Int] = ArrayBuffer.empty[Int]

        val producer: Thread = thread("Producer", producerBody( scenario.numberExchanged, scenario.producerBaseSleep, sync, producedElements))
        val consumer: Thread = thread("Consumer", consumerBody( scenario.numberExchanged, scenario.consumerBaseSleep, sync, consumedElements))

        failAfter(10 seconds) {

          if (scenario.consumerBefore) {
            consumer.start
            producer.start
          } else {
            producer.start
            consumer.start
          }

          producer.join
          consumer.join
        }

        consumedElements must be(producedElements)
      }
    }

  }

}