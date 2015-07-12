package com.htmlism.transducercollections

trait Transducer[A, B] {
  def apply[R](red: Reducer[R, B]): Reducer[R, A]

  def foldUsing[R](red: Reducer[R, B]): Reducer[R, A] = apply[R](red)

  def andThen[C](that: Transducer[B, C]): Transducer[A, C] = new Composite(this, that)
}

class Composite[A, B, C](left: Transducer[A, B], right: Transducer[B, C]) extends Transducer[A, C] {
  def apply[R](red: Reducer[R, C]) = left(right(red))
}

class Filter[A](f: A => Boolean) extends Transducer[A, A] {
  def apply[R](red: Reducer[R, A]) =
    (acc, x) =>
      if (f(x))
        red(acc, x)
      else
        acc
}

class Mapper[A, B](f: A => B) extends Transducer[A, B]  {
  def apply[R](red: Reducer[R, B]) =
    (acc, x) => red(acc, f(x))
}

class FlatMapper[A, B](f: A => TraversableOnce[B]) extends Transducer[A, B]  {
  def apply[R](red: Reducer[R, B]) =
    (acc, x) => f(x).foldLeft(acc)(red)
}

object Filter {
  def apply[A](f: A => Boolean): Filter[A] = new Filter(f)
}

object Mapper {
  def apply[A, B](f: A => B): Mapper[A, B] = new Mapper(f)
}

object FlatMapper {
  def apply[A, B](f: A => TraversableOnce[B]): FlatMapper[A, B] = new FlatMapper(f)
}

