package net.yoshinorin.fpinscala.errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(r) => Right(f(r))
    case Left(l) => Left(l)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(r) => f(r)
    case Left(l) => Left(l)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(r) => Right(r)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      x <- this
      y <- b
    } yield (f(x, y))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def main(args: Array[String]): Unit = {
    // Nothing to to do
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) {
      Left("mean of empty list!")
    } else {
      Right(xs.sum / xs.length)
    }
  }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }

}
