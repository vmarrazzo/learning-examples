package ch3

import java.io.File

import scala.language.postfixOps

import org.scalatest.FlatSpec
import org.scalatest.MustMatchers
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.BeforeAndAfter
import org.scalacheck.Arbitrary._

import org.learningconcurrency.log

import ch3ex8._

class Test_ch3ex8 extends FlatSpec
    with BeforeAndAfter
    with MustMatchers
    with GeneratorDrivenPropertyChecks {

  behavior of "ch3ex8"

  /**
   *
   */
  val serializeBasePath = System.getProperty("java.io.tmpdir") + File.separator

  /**
   * Block that complete correctly
   */
  val blockPass: Function0[Int] =

    () => {
      scala.util.Random.nextInt(100)
    }

  /**
   * Block that generate an exception
   */
  val blockFail: Function0[Int] =

    () => {
      throw new IllegalArgumentException(s"This time we have an error ${scala.util.Random.nextInt(100)}!")
    }

  /**
   * Seed generator for file .ser
   */
  def seedGenerator: String = scala.util.Random.alphanumeric.take(5).mkString

  /**
   *
   */
  it should "serialize and de-serialize in the same JVM" in {

    forAll(Gen.oneOf[Function0[Int]](blockPass, blockFail)) {
      (block0: Function0[Int]) =>
        {

          val seed = seedGenerator

          log(s"Invoke under test code with seed $seed")

          val blockSerialize = s"${serializeBasePath}BlockSer-${seed}.ser"
          val responseSerialize = s"${serializeBasePath}ResponseSer-${seed}.ser"

          /**
           * Core test
           */

          val deSerialized = try {

            //val block : Function0[Int] = () => block2to0(isError,data)

            serializeBlock[Int](block0, blockSerialize)

            executeCarriedBlock[Int](blockSerialize, responseSerialize)

            deSerializeResult(responseSerialize)

          } catch {

            case t: Exception => {
              t.printStackTrace()
              fail("error during core test!")
            }
          }

          /**
           * execute de-serialized block
           */

          deSerialized match {
            case Some(CarrierResult(x)) =>
              x match {
                case error: Throwable => log(s"after serial computation returns $error")
                case value: Int       => log(s"after serial computation returns $value")
              }
            case None => fail("de-serialize does not find a CarrierResult!")
          }

        }
    }

  }

  /**
   *
   */
  it should "serialize and de-serialize in different JVMs" in {

    val block0 = blockFail
    
    /**
     * Core test
     */

    try {

      try {
        log(s"the spawn returns ${spawn(block0)}")  
      }catch {
        case iae: IllegalArgumentException => log(s"the spawn returns ${iae.getMessage}")  
      }
      
    } catch {

      case t: Exception => {
        t.printStackTrace()
        fail("error during core test!")
      }
    }

  }

  /**
   *
   */
  after {

    import java.io.File
    import scala.sys.process._

    for (file2delete <- Seq("find", serializeBasePath, "-name", "*.ser", "-type", "f").lineStream)
      s"rm -rf $file2delete".!
  }
}
