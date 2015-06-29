package ch2

import org.learningconcurrency.log

object ch2ex6 {

  /**
   * 
   */
  class SyncQueue[T]( final val size: Int) {

    sealed trait SyncQueueStatus
    case object Empty extends SyncQueueStatus
    case object NotEmpty extends SyncQueueStatus
    case object Full extends SyncQueueStatus

    import scala.collection.mutable._

    private var values: Queue[T] = Queue.empty[T]

    private var status: SyncQueueStatus = Empty

    def get(): T = status match {
      case Empty => throw new IllegalStateException("SyncQueue is Empty!")
      case _ => {
        val result = values.dequeue
        if (values.isEmpty)
          status = Empty
        else if (values.size == size - 1)
          status = NotEmpty
        result
      }
    }
    def put(x: T): Unit = status match {
      case Full => throw new IllegalStateException("SyncQueue is Full!")
      case _ => {
        values += x
        if (values.size == size)
          status = Full
        else if (values.size == 1)
          status = NotEmpty
      }
    }

    def isEmpty = status.equals(Empty)

    def nonEmpty = !isEmpty

    def isFull = status.equals(Full)

    def getWait(): T = this.synchronized {

      val result = status match {
        case Empty => {
          log("enters into wait state")
          this.wait()
          log("exists from wait state")
          get
        }
        case _ => get
      }

      log("invokes notify")
      this.notify()

      result
    }

    def putWait(x: T): Unit = this.synchronized {

      status match {
        case Full => {
          log("enters into wait state")
          this.wait()
          log("exists from wait state")
          put(x)
        }
        case _ => put(x)
      }

      log("invokes notify")
      this.notify()
    }
  }

}