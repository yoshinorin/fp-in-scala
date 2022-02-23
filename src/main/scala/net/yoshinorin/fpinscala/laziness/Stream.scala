package net.yoshinorin.fpinscala.laziness

import net.yoshinorin.fpinscala.laziness.Stream.empty

import scala.annotation.tailrec

trait Stream[+A] {

  def toList: List[A] = {
    @tailrec
    def run(s: Stream[A], accm: List[A]): List[A] = s match {
      case Cons(h, t) => run(t(), h() :: accm)
      case _ => accm
    }
    run(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n == 1 => Stream.cons(h(), empty)
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => Stream.cons(h(), t().takeWhile(f))
    case _ => empty
  }

  def exists(f: A => Boolean): Boolean = this match {
    // f(h()) がtrueを返した場合に以降は評価されない
    case Cons(h, t) => f(h()) || t().exists(f)
    case _ => false
  }

  // f の第二パラメータは非正格
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // スマートコンストラクタ
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) {
      empty
    } else {
      // 引数はサンクにまとめられる。評価されるまで評価されない
      cons(as.head, apply(as.tail: _*))
    }
  }
}
