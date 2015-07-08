package ch3

import java.io._
import scala.sys.process._

import org.learningconcurrency.log

object ch3ex8 {

  /**
   * The carrier for code block
   */
  case class CarrierBlock[T](block: () => T) extends scala.Serializable

  /**
   * The carrier for result
   */
  case class CarrierResult[T](result: Any) extends scala.Serializable

  /**
   * The remote executor based on serialization
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
   * To be executed on spawned JVM
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
      case None => ???
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

    try {
      val ois = new ObjectInputStream(new FileInputStream(blockSerializedPath))
      val deSerialized = ois.readObject.asInstanceOf[CarrierBlock[T]]
      ois.close

      Some(deSerialized.block)
    } catch {
      case t: Throwable => {
        val message = s"error during block de-serialize -> $blockSerializedPath"
        log(message)
        None
        throw new IOException(message)
      }
    }
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