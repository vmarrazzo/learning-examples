package ch2

import ch2ex8._

object ch2ex9 {

  class PriorityTaskPool4MoreWorkers(val numbWorkers: Int) extends AbstractPriorityTaskPool {

    override def init() = {

      /**
       * with "until" we generate one missing worker that will be the parent one
       */
      val workers = for (i <- 1 to numbWorkers) yield new Worker(s"Worker_${i}")
      for (w <- workers) w.start
    }

  }
}