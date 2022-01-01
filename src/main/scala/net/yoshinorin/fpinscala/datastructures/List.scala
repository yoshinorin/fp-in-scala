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

  def drop[A](l: List[A], n: Int = 0): List[A] = {
    if (n == 0) return l
    l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
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

    // drop
    println("\n-------drop")
    println(drop(List(1, 2, 3, 4), 2))
    println(drop(List()))
    println(drop(List(), 2))
    println(drop(List("A", "B", "C"), 2))
    println(drop(List("A", "B", "C"), 4))
  }

}
