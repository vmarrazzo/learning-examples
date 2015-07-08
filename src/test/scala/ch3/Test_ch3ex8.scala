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
   *
   */
  it should "serialize and de-serialize" in {

    import ch3ex8._

    val blockPass: Function0[Int] =

      () => {
        scala.util.Random.nextInt(100)
      }

    val blockFail: Function0[Int] =

      () => {
        throw new IllegalArgumentException(s"This time we have an error ${scala.util.Random.nextInt(100)}!")
      }

    forAll(Gen.oneOf[Function0[Int]](blockPass, blockFail)) {
      (block0: Function0[Int]) =>
        {

          val seed: String = scala.util.Random.alphanumeric.take(5).mkString

          val blockSerialize = s"${serializeBasePath}BlockSer-${seed}.ser"
          val responseSerialize = s"${serializeBasePath}ResponseSer-${seed}.ser"

          try {

            log(s"Invoke under test code with seed $seed")

            //val block : Function0[Int] = () => block2to0(isError,data)

            ch3ex8.serializeBlock[Int](block0, blockSerialize)

            ch3ex8.executeCarriedBlock[Int](blockSerialize, responseSerialize)

            val deSerialized = ch3ex8.deSerializeResult(responseSerialize)

            deSerialized match {
              case Some(CarrierResult(x)) => 
                x match {
                  case error : Throwable => log(s"after serial computation returns $error")
                  case value : Int => log(s"after serial computation returns $value")
                }
              case None => fail("de-serialize must returns a CarrierResult!")
            }

            true must be(true)

          } catch {

            case t: Exception => {
              t.printStackTrace()
              fail("de-serialize must returns a CarrierResult!")
            }
          }

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
