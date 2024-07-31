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

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
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

  def reverse[A](ls: List[A]): List[A] = {
    // foldLeft(ls, List())((acc, h) => Cons(h, acc)) だと型推論できない（？）のかコンパイルエラーになる
    foldLeft(ls, List[A]())((acc, h) => Cons(h, acc))
  }

  def incrementOne(ls: List[Int]): List[Int] = {
    foldRight(ls, Nil: List[Int])((i, acc) => Cons(i + 1, acc))
    // reverse(foldLeft(ls, Nil: List[Int])((acc, i) => Cons(i + 1, acc)))
  }

  def toString(ls: List[Double]): List[String] = {
    foldRight(ls, Nil: List[String])((d, acc) => Cons(d.toString, acc))
  }

  def map[A, B](ls: List[A])(f: A => B): List[B] = {
    foldRight(ls, Nil: List[B])((x, xs) => Cons(f(x), xs))
  }

  def filter[A](ls: List[A])(f: A => Boolean): List[A] = {
    foldRight(ls, Nil: List[A])((x, y) => {
      if (f(x)) {
        y
      } else {
        Cons(x, y)
      }
    })
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filterViaFlatMap[A, B](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => {
      if (f(x)) {
        Nil
      } else {
        List(x)
      }
    })
  }

  def addPairList(ls1: List[Int], ls2: List[Int]): List[Int] = {
    (ls1, ls2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, b), Cons(c, d)) => Cons(a + c, addPairList(b, d))
    }
  }

  def zipWith[A, B, C](ls1: List[A], ls2: List[B])(f: (A, B) => C): List[C] = {
    (ls1, ls2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, b), Cons(c, d)) => Cons(f(a, c), zipWith(b, d)(f))
    }
  }

  def main(args: Array[String]): Unit = {
    // Nothing to to do
  }
}
