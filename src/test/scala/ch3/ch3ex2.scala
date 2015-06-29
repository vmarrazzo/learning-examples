package ch3

import java.util.LinkedList
import java.util.concurrent.atomic.AtomicReference

import org.learningconcurrency.log

import scala.annotation._

object ch3ex2 {

  class TreiberStack[T] {

    private val innerStack = new LinkedList[T]
    private val stack: AtomicReference[LinkedList[T]] = new AtomicReference(innerStack)

    @tailrec private def addHead(x: T): Unit = {

      val xs = stack.get
      val nxs = new LinkedList[T](xs)
      nxs.addFirst(x)
      if (!stack.compareAndSet(xs, nxs)) {
        log("CAS operation on PUSH fail -> retry")
        this.addHead(x)
      }
    }

    @tailrec private def removeHead(): T = {

      val xs = stack.get
      val nxs = new LinkedList[T](xs)
      val resp = nxs.removeFirst
      if (stack.compareAndSet(xs, nxs))
        resp
      else {
        log("CAS operation on POP fail -> retry")
        this.removeHead
      }
    }

    def push(x: T): Unit = addHead(x)

    def pop(): T = removeHead

    override def toString = innerStack.toString
  }
}