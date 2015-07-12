package com.htmlism.transducercollections

trait Transducer[A, B] {
  def apply[R](f: Reducer[R, B]): Reducer[R, A]

  def foldUsing[R](f: Reducer[R, B]): Reducer[R, A] = apply[R](f)

  def andThen[C](that: Transducer[B, C]): Transducer[A, C] = new Composite(this, that)
}

class Composite[A, B, C](left: Transducer[A, B], right: Transducer[B, C]) extends Transducer[A, C] {
  def apply[R](f: Reducer[R, C]) = left(right(f))
}

class Filter[A](f: A => Boolean) extends Transducer[A, A] {
  def apply[R](g: Reducer[R, A]) =
    (r, a) =>
      if (f(a))
        g(r, a)
      else
        r
}

class Mapper[A, B](f: A => B) extends Transducer[A, B]  {
  def apply[R](g: Reducer[R, B]) =
    (r, a) => g(r, f(a))
}

class FlatMapper[A, B](f: A => TraversableOnce[B]) extends Transducer[A, B]  {
  def apply[R](g: Reducer[R, B]) =
    (r, a) => f(a).foldLeft(r)(g)
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

