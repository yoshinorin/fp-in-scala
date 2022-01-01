package net.yoshinorin.fpinscala.datastructures

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
  }

}
