package ch2

import scala.collection.mutable._

import org.learningconcurrency.log

object ch2ex8 {

  //  1 -> highest
  // 10 -> lowest
  case class PriorityTask(body: () => Unit, priority: Int)

  /**
   * Base abstract class for PriorityTaskPool
   */
  abstract class AbstractPriorityTaskPool {

    val highestPriority = 1
    val lowestPriority = 10

    /**
     * Ordering logic
     */
    implicit val ord = new Ordering[PriorityTask] {
      def compare(a: PriorityTask, b: PriorityTask) = b.priority - a.priority
    }

    /**
     * Priority queue
     */
    implicit val tasks = PriorityQueue[PriorityTask]()

    /**
     *
     */
    def asynchronous(priority: Int)(task: => Unit): Unit = tasks.synchronized {

      priority match {
        case p if highestPriority to lowestPriority contains p => {
          tasks.enqueue(PriorityTask(() => task, p))
          tasks.notify()
        }
        case _ => throw new IllegalArgumentException(s"Priority value $priority cannot be accepted because out of range ($highestPriority,$lowestPriority)")
      }

    }

    /**
     * Implicit worker default body
     */
    implicit val defaultWorkerRunBody: PriorityQueue[PriorityTask] => Unit =
      t => {

        def poll() = t.synchronized {
          while (t.isEmpty) t.wait()
          val pTask = t.dequeue()
          log(s"dequeued a task with priority ${pTask.priority}")
          pTask.body
        }

        while (true) {
          val task = poll()
          task()
        }
      }

    /**
     * Worker class
     */
    class Worker(name: String)(implicit tasks: PriorityQueue[PriorityTask], workerRunBody: PriorityQueue[PriorityTask] => Unit) extends Thread {

      setName(name)
      setDaemon(true)

      override def run() = workerRunBody(tasks)
    }

    /**
     * Init method to be implemented in final class
     */
    def init(): Unit

    /**
     * Start the worker
     */
    init
  }

  /**
   * Concrete class
   */
  class PriorityTaskPool extends AbstractPriorityTaskPool {

    override def init() = new Worker("PriorityTaskPool-Worker").start()
  }
}