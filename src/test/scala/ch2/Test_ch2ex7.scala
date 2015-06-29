package ch2

import scala.concurrent.duration._
import org.scalatest.{ FlatSpec, MustMatchers }
import org.scalatest.concurrent.Timeouts
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.language.postfixOps
import org.scalacheck.Gen

class Test_ch2ex7 extends FlatSpec
  with MustMatchers
  with Timeouts
  with GeneratorDrivenPropertyChecks {

  behavior of "ch2ex7"

  it should "works using two thread and scalacheck" in {

    val numberAccounts = 100
  
    import ch2ex7._
    
    val gen : Gen[Int] = Gen.choose(1, numberAccounts)

    implicit val printLog : Boolean = false
    
    val accounts : Set[Account] = Set.empty[Account]

    forAll( (gen, "from"), (gen, "to")) { (fromIndex: Int, toIndex: Int) =>

      whenever( toIndex - fromIndex > 0 ) {

        // necessary precondition for each test
        resetUniqueId()
        
        val accounts = (for (i <- (1 to numberAccounts)) yield new Account(s"Account$i", 1000)).toSet

        val choosenDonors = accounts.filter( (a: Account) => {a.uid >= fromIndex && a.uid <= toIndex} )

        withClue(s"${choosenDonors} size check : ") { choosenDonors must have size (toIndex-fromIndex+1) }
        
        val target1: Account = new Account(s"AccountTarget1", 1000)
        val target2: Account = new Account(s"AccountTarget2", 1000)

        failAfter(1 seconds) {

          val t1 = thread("Banker1", { for (i <- 0 until 100) sendAll(choosenDonors, target1) })
          val t2 = thread("Banker2", { for (i <- 0 until 100) sendAll(choosenDonors, target2) })

          t1.start
          t2.start

          t1.join
          t2.join
        }

        // check that account not involved maintains untouched its money
        for (i <- accounts -- choosenDonors ) {
          withClue(s"${i.name} : ") { (i.money) must be(1000)  }
        }
        
        // check that donors have money to zero
        for (i <- choosenDonors) {
          withClue(s"${i.name} : ") { (i.money) must be(0)  }
        }
        
        // the expected money into only one Banker thread
        val expectedMoney = ( (toIndex-fromIndex+1) + 1 ) * 1000
        
        // check that final money into Banker thread are coherent as expected
        // only one thread can move the total amount and the other remains unchanged
        List(target1.money, target2.money) must contain (expectedMoney)
        List(target1.money, target2.money) must contain (1000)
      }
    }

  }
}