package com.htmlism.transducercollections

/**
  * Companion object for creating custom collections that are at least foldable.
  */

object Foldable {
  /**
    * Constructs a foldable custom collection given an inline Scala sequence.
    *
    * @param xs
    * @tparam A
    * @return
    */

  def apply[A](xs: A*): Foldable[A] = new Foldable(xs)
}

trait StreamLike[A, B] extends CanFold[A] with CanChainOperations[A] with CanConvert[A, B]

/**
  * An illustration of transducer trait use via an in-memory collection.
  *
  * @param xs
  * @tparam A
  */

class Foldable[A](xs: Seq[A]) extends StreamLike[A, A] {
  def fold[B](z: B, f: (B, A) => B): B = xs.foldLeft(z)(f)

  def toList: List[A] = fold[List[A]](Nil, (acc, x) => acc :+ x)
}

class WrappedFoldable[A, B](xs: CanFold[A], t: Transducer[A, B]) extends StreamLike[A, B] {
  def fold[B](z: B, f: (B, A) => B): B = xs.fold(z, f)

  def toList: List[B] = fold[List[B]](Nil, t((acc, x) => acc :+ x))
}

/**
  * A custom trait to describe a structure that can be folded over.
  *
  * @tparam A The type of the members in the structure
  */

trait CanFold[A] {
  /**
    * Accumulates singular values of A into a composite of B.
    *
    * @param z The initial value of B (the "zero")
    * @param f The reducing function
    * @tparam B The composite result
    * @return A composite result
    */

  def fold[B](z: B, f: (B, A) => B): B
}

/**
  * Describes a structure that supports common functional operations.
  *
  * @tparam A The type of individual units being operated upon
  */

trait CanChainOperations[A] {
  // demands that the user of this class must also support folding
  self: CanFold[A] =>

  /**
    * Adds a layer of filtering to the foldable construct.
    *
    * @param f
    * @return
    */

  def filter(f: A => Boolean): WrappedFoldable[A, A] =
    new WrappedFoldable(this, Filter(f))

  /**
    * Adds a layer of mapping to the foldable construct.
    *
    * @param f
    * @return
    */

  def map[B](f: A => B): WrappedFoldable[A, B] =
    new WrappedFoldable(this, Mapper(f))

  /**
    * Adds a layer of flat mapping to the foldable construct.
    *
    * @param f
    * @return
    */

  def flatMap[B](f: A => TraversableOnce[B]): WrappedFoldable[A, B] =
    new WrappedFoldable(this, FlatMapper(f))
}

/**
  * Describes a structure that can be converted back to a normal Scala collection.
  *
  * @tparam A
  * @tparam B
  */

trait CanConvert[A, B] {
  self: CanFold[A] =>

  def toList: List[B]
}
