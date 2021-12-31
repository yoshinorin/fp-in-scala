package net.yoshinorin.fpinscala.gettingstarted

import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatResult("abs", -5, abs))
    println(formatResult("factorial", 5, factorial))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, c: Int): Int = {
      if (n == 0) {
        prev
      } else {
        loop(n - 1, c, prev + c)
      }
    }
    loop(n, 0, 1)
  }
}
