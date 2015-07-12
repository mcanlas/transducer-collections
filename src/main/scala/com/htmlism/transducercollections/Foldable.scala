package com.htmlism.transducercollections

object Foldable {
  def apply[A](xs: A*) = new Foldable(xs)
}

class Foldable[A](xs: Seq[A]) extends CanFold[A] with CanChain[A] {
  def fold[B](z: B, f: (B, A) => B): B = xs.foldLeft(z)(f)
}

class WrappedFoldable[A, B](xs: CanFold[A], t: Transducer[A, B]) extends CanFold[A] with CanChain[A] {
  def fold[B](z: B, f: (B, A) => B): B = xs.fold(z, f)

  def render = xs.fold[List[B]](Nil, t((acc, x) => acc :+ x))
}

trait CanFold[A] {
  def fold[B](z: B, f: (B, A) => B): B
}

trait CanChain[A] {
  self: CanFold[A] =>

  def filter(f: A => Boolean): WrappedFoldable[A, A] =
    new WrappedFoldable(this, Filter(f))

  def map[B](f: A => B): WrappedFoldable[A, B] =
    new WrappedFoldable(this, Mapper(f))

  def flatMap[B](f: A => TraversableOnce[B]): WrappedFoldable[A, B] =
    new WrappedFoldable(this, FlatMapper(f))
}
