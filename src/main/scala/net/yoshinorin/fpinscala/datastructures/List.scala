package net.yoshinorin.fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def setHead[A](v: A, l: List[A]): List[A] = {
    l match {
      case Nil => Cons(v, Nil)
      case Cons(x, xs) => Cons(v, xs)
    }
  }

  @tailrec
  def drop[A](l: List[A], n: Int = 0): List[A] = {
    if (n == 0) return l
    l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  /*
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      // こちらが正しいハズだが、判定を逆転させないと想定通りに動かない...
      //case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case Cons(x, xs) if !f(x) => dropWhile(xs, f)
      case _ => l
    }
  }
   */

  // こうすることでカリー化される
  // 呼び出し側は第2引数の型アノテーションが不要になる
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if !f(x) => dropWhile(xs)(f)
      case _ => l
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)((x, y) => x + y)
  }

  def product2(ns: List[Int]) = {
    foldRight(ns, 1.0)(_ * _) // (_ * _) は (x, y) => x * y の簡易表記
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  def foldLeft[A, B](l: List[A], acc: B)(f: (B, A) => B): B = {
    l match {
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h))(f)
    }
  }

  def sum3(ns: List[Int]): Int = {
    foldLeft(ns, 0)(_ + _)
  }

  def product3(ns: List[Double]): Double = {
    foldLeft(ns, 1.0)(_ * _)
  }

  def main(args: Array[String]): Unit = {
    // tail
    println("\n-------tail")
    println(tail(List(1, 2, 3, 4)))
    println(tail(List()))
    println(tail(List("A", "B", "C")))

    // setHead
    println("\n-------setHead")
    println(setHead(1, List(2, 3, 4, 5)))
    println(setHead(1, List()))
    println(setHead("A", List("B", "C", "D")))

    // drop
    println("\n-------drop")
    println(drop(List(1, 2, 3, 4), 2))
    println(drop(List()))
    println(drop(List(), 2))
    println(drop(List("A", "B", "C"), 2))
    println(drop(List("A", "B", "C"), 4))

    // dropWhile
    println("\n-------dropWhile")
    //println(dropWhile(List(1, 3, 2, 3, 5), (x: Int) => x == 3))  //
    println(dropWhile(List(1, 3, 2, 3, 5))(x => x == 3))

    // exercise 3.8
    //println(foldRight(List(1, 2, 3), Nil)(Cons(_, _)))

    // length
    println("\n-------length")
    println(length(List("A", "B", "C", "D")))

    // foldLeft - sum
    println("\n-------foldleft sum")
    println(sum3(List(2, 3, 45)))
    println(product3(List(2, 2, 3)))
  }
}
