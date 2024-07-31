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
    println(findFirst(Array(1, 12, 32, 43, 50), (x: Int) => x == 32))
    println(findFirst(Array(1, 12, 32, 43, 50), (x: Int) => x == 99))
    println(isSorted(Array(), (x: Int, y: Int) => x > y))
    println(isSorted(Array(1), (x: Int, y: Int) => x > y))
    println(isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x >= y))
    println(isSorted(Array(1, 3, 2, 4), (x: Int, y: Int) => x >= y))
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

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def compare(x: Int): Boolean = {
      if (x >= as.length - 1) true
      else if (gt(as(x), as(x + 1))) false
      else compare(x + 1)
    }
    compare(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { a => b => f(a, b) }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = { (a, b) => f(a)(b) }

  def compose[A, B, C](f: B => C, g: A => B): A => C = { a => f(g(a)) }
}
