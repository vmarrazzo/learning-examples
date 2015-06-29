package ch2

import ch2.ch2ex9.PriorityTaskPool4MoreWorkers
import org.scalatest.FlatSpec

class Test_ch2ex9 extends FlatSpec with AbstractTest {

  override var numberOfTasks = 10
  
  behavior of "ch2ex9"
  
  it should "works using scalacheck" in {

    coreTest[PriorityTaskPool4MoreWorkers](new PriorityTaskPool4MoreWorkers(5))
  }
}