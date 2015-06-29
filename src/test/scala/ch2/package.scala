import org.learningconcurrency._

package object ch2 {

  /**
   * Create a thread with passed body
   */
  def thread(name: String, body: => Unit)(implicit printLog : Boolean = true): Thread = {

    val t = new Thread {

      override def run() = {
        if (printLog) log("Start")
        body
        if (printLog) log("Stop")
      }
    }

    t.setName(name)
    t
  }

}
