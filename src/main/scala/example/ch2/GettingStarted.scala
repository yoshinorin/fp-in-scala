package example.ch2

object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) {
      -n
    } else {
      n
    }
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // 単相関数
  private def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if(n >= ss.length) {
        -1
      } else if (ss(n) == key) {
        n
      } else {
        loop(n + 1)
      }
    }

    loop(0)
  }

  // 多相関数（総称関数）
  private def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if(n >= as.length) {
        -1
      } else if (as(n) == p) {
        n
      } else {
        loop(n + 1)
      }
    }

    loop(0)
  }


  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) {
        true
      } else if (gt(as(n), as(n + 1))) {
        false
      } else {
        go(n + 1)
      }
    }

    go(0)
  }


  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))

    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }


  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) {
        acc
      } else {
        go(n - 1, n * acc)
      }
    }
    go(n, 1)
  }

  def fib(n: Int): Int = {
    def go(n: Int, current: Int, next: Int): Int = {
      if (n <= 0) {
        current
      } else {
        go(n - 1, next, current + next)
      }
    }
    go(0, 0, 1)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    // (b: B) => f(a, b)
    // コンテキストから `b` の型を把握しているので `(b: B)` と記述する必要はない
    b => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}
