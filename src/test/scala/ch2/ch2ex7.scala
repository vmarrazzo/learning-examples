package ch2

object ch2ex7 {

  private var uidCount = 0

  def resetUniqueId() = this.synchronized {
    uidCount = 0
  }
  
  def getUniqueId() = this.synchronized {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  class Account(val name: String, var money: Int) {
    val uid = getUniqueId()

    override def toString = s"$name : $uid : $money"
  }

  def send(a: Account, b: Account, n: Int) = {

    def move = {
      a.money -= n
      b.money += n
    }

    if (a.uid < b.uid)
      a.synchronized {
        b.synchronized {
          move
        }
      }
    else
      b.synchronized {
        a.synchronized {
          move
        }
      }

  }

  /**
   *
   */
  def sendAll(accounts: Set[Account], target: Account): Unit = {

    /**
     * This method moves all money from not-target account to target account.
     * 
     * It supposes that all account synchronization is acquired.
     */
    def actOnSynched(synchedAccount: List[Account]) = {
      for (from <- synchedAccount.filter(!_.equals(target))) {
          val amount = from.money
          from.money -= amount
          target.money += amount
      }
    }

    /**
     *
     */
    def syncAllAndMove(synchedAccount: List[Account], subListAccount: List[Account]): Unit = {

      subListAccount match {
        case Nil          => actOnSynched(synchedAccount)
        case head :: tail => head.synchronized { syncAllAndMove(synchedAccount :+ head, tail) }
      }

    }

    val allSortedAccounts: List[Account] = (accounts + target).toList.sortWith((a1: Account, a2: Account) => a1.uid < a2.uid)

    syncAllAndMove(Nil, allSortedAccounts)
  }

}
