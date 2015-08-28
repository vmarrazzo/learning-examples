package ch5

import org.scalatest.FlatSpec

import org.learningconcurrency.ch5._

class Test_ch5ex1 extends FlatSpec {
  
  behavior of "ch5ex1"
  
  it should "generates should measure average allocation time" in {
    
    val numbers = 10000
    val rnd = scala.util.Random
    
    import breeze.math.Complex
    
    def allocBody = Complex( rnd.nextDouble, rnd.nextDouble)

    val avg = warmedTimed(numbers)( allocBody )
    
    info(s"Average on $numbers allocations is $avg ns")
  }
}