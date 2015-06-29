package ch2

import org.learningconcurrency.log

object ch2ex5 {

  /**
   * 
   */
  class SyncVar[T] {

    sealed trait SyncVarStatus
    case object Empty extends SyncVarStatus
    case object NotEmpty extends SyncVarStatus

    private var value: T = _

    private var status: SyncVarStatus = Empty

    def get(): T = status match {
      case Empty => throw new IllegalStateException("SyncVar is Empty!")
      case NotEmpty => {
        status = Empty
        value
      }
    }
    def put(x: T): Unit = status match {
      case Empty => {
        value = x
        status = NotEmpty
      }
      case NotEmpty => throw new IllegalStateException("SyncVar is NotEmpty!")
    }

    def isEmpty = status.equals(Empty)

    def nonEmpty = !isEmpty

    def getWait(): T = this.synchronized {

      val result = status match {
        case Empty => {
          log("enters into wait state")
          this.wait()
          log("exists from wait state")
          get
        }
        case NotEmpty => get
      }

      log("invokes notify")
      this.notify()

      result
    }

    def putWait(x: T): Unit = this.synchronized {

      status match {
        case Empty => put(x)
        case NotEmpty => {
          log("enters into wait state")
          this.wait()
          log("exists from wait state")
          put(x)
        }
      }

      log("invokes notify")
      this.notify()
    }
  }

}