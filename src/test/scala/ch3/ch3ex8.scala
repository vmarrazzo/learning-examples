package ch3

import java.io._
import scala.sys.process._

import org.learningconcurrency.log

object ch3ex8 {

  /**
   *
   */
  def spawn[T](block: => T): T = {

    val seed = scala.util.Random.alphanumeric.take(5).mkString

    val serializeBasePath = System.getProperty("java.io.tmpdir") + File.separator

    log(s"Invoke under test code with seed $seed")

    val blockSerialize = s"${serializeBasePath}BlockSer-${seed}.ser"
    val responseSerialize = s"${serializeBasePath}ResponseSer-${seed}.ser"

    serializeBlock[T](() => block, blockSerialize)

    val fileBlock = new File(blockSerialize)
    
    do {
      Thread.sleep(500)  
    } while ( !fileBlock.exists )
    
    ///  JVM
    val scalaBin = Seq("whereis", "scala").!!.trim.split(" ")(1)
    
    //val debug = """export JAVA_OPTS="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=4000,server=y,suspend=y";"""
    
    val command = s"$scalaBin -cp ./target/scala-2.11/test-classes/:./target/scala-2.11/classes/ ch3.ch3ex8 ${blockSerialize} ${responseSerialize}"
    
    val remoteProcess = command.run()
    
    val fileResponse = new File(responseSerialize)
    
    do {
      Thread.sleep(500)  
    } while ( !fileResponse.exists )
    
    /**
     * scala -cp ./target/scala-2.11/test-classes/:./target/scala-2.11/classes/ ch3.ch3ex8 ${blockSerialize} ${responseSerialize}
     */
    
    deSerializeResult(responseSerialize) match {
      case Some(CarrierResult(x)) =>
        x match {
          case error: Throwable => throw error
          case value: T       => value
        }
      case None => ???
    }
  }

  /**
   * The carrier for code block
   */
  case class CarrierBlock[T](block: () => T) extends scala.Serializable

  /**
   * The carrier for result
   */
  case class CarrierResult[T](result: Any) extends scala.Serializable

  /**
   * To be executed on spawned JVM
   */
  def main(args: Array[String]) {

    args.toList match {
      case carrierBlockPath :: carrierResultPath :: Nil => {

        log(s"carrierBlockPath -> ${carrierBlockPath}")
        log(s"carrierResultPath -> ${carrierResultPath}")

        executeCarriedBlock(carrierBlockPath, carrierResultPath)
      }
      case _ => {
        log(s"Missing input parameter!!! -> ${args.toList}")
      }
    }
  }

  /**
   * This method de-serialize block and execute it. It serialize the response object.
   *
   * It can throw an exception on serialize issue
   */
  def executeCarriedBlock[T](carrierBlockPath: String, carrierResultPath: String): Unit = {

    deSerializeBlock[T](carrierBlockPath) match {
      case Some(x) => {

        serializeResponse(try {
          x()
        } catch {
          case t: Throwable => t
        }, carrierResultPath)
      }
      case None => {
        val message = s"error during block de-serialize : nothing is returned!"
        log(message)
        throw new IOException(message)
      }
    }

  }

  /**
   * This method serialize the block of code.
   *
   * On serialization error throws an IOException
   */
  def serializeBlock[T](block: Function0[T], blockSerializationPath: String): Unit = {

    val toBeSerialized = new CarrierBlock[T](block)

    try {
      val oos = new ObjectOutputStream(new FileOutputStream(blockSerializationPath))
      oos.writeObject(toBeSerialized)
      oos.close
    } catch {
      case nse: NotSerializableException => {
        val message = s"error during block serialize -> $blockSerializationPath"
        log(message)
        throw new IOException(message)
      }
    }

  }

  /**
   * This method handles de-serialize CarrierBlock
   *
   * On serialization error throws an IOException
   */
  def deSerializeBlock[T](blockSerializedPath: String): Option[() => T] = {

    val deSerialized = try {
      val ois = new ObjectInputStream(new FileInputStream(blockSerializedPath))
      val obj = ois.readObject.asInstanceOf[CarrierBlock[T]]
      ois.close

      obj
    } catch {
      case t: Throwable => {
        val message = s"error during block de-serialize -> $blockSerializedPath"
        log(message)
        throw new IOException(message)
      }
    }

    if (deSerialized.block.isInstanceOf[Function0[T]])
      Some(deSerialized.block)
    else
      throw new IOException("Error block deserialized type missmatch!")
  }

  /**
   * This method serialize response (or error on execution)
   *
   * On serialization error throws an IOException
   */
  def serializeResponse[T](toSerializeBack: Any, responseSerializationPath: String): Unit = {

    try {
      val oos = new ObjectOutputStream(new FileOutputStream(responseSerializationPath))
      oos.writeObject(CarrierResult(toSerializeBack))
      oos.close
    } catch {
      case nse: NotSerializableException => {
        val message = s"error during response serialize -> $responseSerializationPath"
        log(message)
        throw new IOException(message)
      }
    }

  }

  /**
   * This method handles de-serialize CarrierResult
   *
   * On serialization error throws an IOException
   */
  def deSerializeResult[T](serializedResponse: String): Option[CarrierResult[T]] = {

    try {
      val ois = new ObjectInputStream(new FileInputStream(serializedResponse))
      val deSerialized = ois.readObject.asInstanceOf[CarrierResult[T]]
      ois.close

      Some(deSerialized)
    } catch {
      case ie: IOException => {
        val message = s"error during response de-serialize -> $serializedResponse"
        log(message)
        None
        throw new IOException(message)
      }
    }

  }

}