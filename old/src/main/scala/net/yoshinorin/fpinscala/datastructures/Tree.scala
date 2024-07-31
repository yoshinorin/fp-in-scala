package net.yoshinorin.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def main(args: Array[String]): Unit = {
    // Nothing to to do
  }

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximam(t: Tree[Int]): Int = {
    t match {
      case Leaf(l) => l
      case Branch(l, r) => maximam(l) max maximam(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(l) => Leaf(f(l))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t: Tree[A])(f: A => B)(f2: (B, B) => B): B = {
    t match {
      case Leaf(l) => f(l)
      case Branch(l, r) => f2(fold(l)(f)(f2), fold(r)(f)(f2))
    }
  }

}
